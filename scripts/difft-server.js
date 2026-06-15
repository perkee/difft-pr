const http = require('http');
const { execFileSync } = require('child_process');
const fs = require('fs');
const os = require('os');
const path = require('path');

const PORT = process.env.DIFFT_PORT || 3001;

function fetchGithub(url, token) {
  return new Promise((resolve, reject) => {
    const parsed = new URL(url);
    const options = {
      hostname: parsed.hostname,
      path: parsed.pathname + parsed.search,
      headers: {
        'Authorization': `Bearer ${token}`,
        'Accept': 'application/vnd.github.raw+json',
        'User-Agent': 'difft-pr',
      },
    };
    require('https').get(options, (res) => {
      if (res.statusCode === 404) {
        resolve('');
        return;
      }
      if (res.statusCode < 200 || res.statusCode >= 300) {
        reject(new Error(`GitHub API ${res.statusCode}: ${url}`));
        return;
      }
      let data = '';
      res.on('data', (chunk) => { data += chunk; });
      res.on('end', () => resolve(data));
    }).on('error', reject);
  });
}

function fetchGithubJson(url, token) {
  return new Promise((resolve, reject) => {
    const parsed = new URL(url);
    const options = {
      hostname: parsed.hostname,
      path: parsed.pathname + parsed.search,
      headers: {
        'Authorization': `Bearer ${token}`,
        'Accept': 'application/json',
        'User-Agent': 'difft-pr',
      },
    };
    require('https').get(options, (res) => {
      if (res.statusCode < 200 || res.statusCode >= 300) {
        reject(new Error(`GitHub API ${res.statusCode}: ${url}`));
        return;
      }
      let data = '';
      res.on('data', (chunk) => { data += chunk; });
      res.on('end', () => {
        try { resolve(JSON.parse(data)); }
        catch (e) { reject(e); }
      });
    }).on('error', reject);
  });
}

function runDifft(lhsContent, rhsContent, filename) {
  const tmpDir = fs.mkdtempSync(path.join(os.tmpdir(), 'difft-'));
  const ext = path.extname(filename) || '.txt';
  const lhsPath = path.join(tmpDir, `a${ext}`);
  const rhsPath = path.join(tmpDir, `b${ext}`);

  try {
    fs.writeFileSync(lhsPath, lhsContent);
    fs.writeFileSync(rhsPath, rhsContent);

    const result = execFileSync('difft', ['--display', 'json', lhsPath, rhsPath], {
      encoding: 'utf-8',
      timeout: 30000,
      env: { ...process.env, DFT_UNSTABLE: 'yes' },
    });
    return JSON.parse(result);
  } catch (e) {
    if (e.stdout) {
      try { return JSON.parse(e.stdout); }
      catch (_) { /* fall through */ }
    }
    return { chunks: [], language: 'Unknown', path: filename, status: 'error' };
  } finally {
    fs.rmSync(tmpDir, { recursive: true, force: true });
  }
}

async function handlePRDiff(req, res) {
  let body = '';
  for await (const chunk of req) body += chunk;

  const { owner, repo, pr_number, token } = JSON.parse(body);

  const pr = await fetchGithubJson(
    `https://api.github.com/repos/${owner}/${repo}/pulls/${pr_number}`,
    token
  );
  const baseSha = pr.base.sha;
  const headSha = pr.head.sha;

  const files = await fetchGithubJson(
    `https://api.github.com/repos/${owner}/${repo}/pulls/${pr_number}/files?per_page=100`,
    token
  );

  const results = [];
  for (const file of files) {
    if (file.status === 'removed') {
      results.push({
        filename: file.filename,
        language: 'Unknown',
        status: 'removed',
        chunks: [],
      });
      continue;
    }

    let baseContent = '';
    if (file.status !== 'added') {
      try {
        baseContent = await fetchGithub(
          `https://api.github.com/repos/${owner}/${repo}/contents/${encodeURIComponent(file.filename)}?ref=${baseSha}`,
          token
        );
      } catch (_) {
        baseContent = '';
      }
    }

    let headContent = '';
    try {
      headContent = await fetchGithub(
        `https://api.github.com/repos/${owner}/${repo}/contents/${encodeURIComponent(file.filename)}?ref=${headSha}`,
        token
      );
    } catch (_) {
      headContent = '';
    }

    if (file.status === 'added') {
      results.push({
        filename: file.filename,
        language: 'Unknown',
        status: 'added',
        chunks: [],
        headContent: headContent,
      });
      continue;
    }

    const diffResult = runDifft(baseContent, headContent, file.filename);
    results.push({
      filename: file.filename,
      language: diffResult.language || 'Unknown',
      status: diffResult.status || file.status,
      chunks: diffResult.chunks || [],
    });
  }

  res.writeHead(200, {
    'Content-Type': 'application/json',
    'Access-Control-Allow-Origin': '*',
  });
  res.end(JSON.stringify(results));
}

const server = http.createServer(async (req, res) => {
  if (req.method === 'OPTIONS') {
    res.writeHead(204, {
      'Access-Control-Allow-Origin': '*',
      'Access-Control-Allow-Methods': 'POST',
      'Access-Control-Allow-Headers': 'Content-Type',
    });
    res.end();
    return;
  }

  if (req.method === 'POST' && req.url === '/pr-diff') {
    try {
      await handlePRDiff(req, res);
    } catch (e) {
      res.writeHead(500, { 'Content-Type': 'application/json' });
      res.end(JSON.stringify({ error: e.message }));
    }
    return;
  }

  res.writeHead(404);
  res.end('Not found');
});

server.listen(PORT, () => {
  console.log(`Difft service running on http://localhost:${PORT}`);
});
