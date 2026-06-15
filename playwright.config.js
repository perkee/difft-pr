// @ts-check
const { defineConfig } = require('@playwright/test');

const baseURL = process.env.BASE_URL || 'http://localhost:8000';

module.exports = defineConfig({
  testDir: './e2e',
  timeout: 30_000,
  retries: process.env.CI ? 1 : 0,
  use: {
    baseURL,
    headless: true,
  },
  projects: [
    { name: 'chromium', use: { browserName: 'chromium' } },
  ],
  ...(process.env.BASE_URL ? {} : {
    webServer: {
      command: 'lamdera live',
      url: 'http://localhost:8000',
      reuseExistingServer: !process.env.CI,
      timeout: 30_000,
    },
  }),
});
