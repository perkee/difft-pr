// @ts-check
const { test, expect } = require('@playwright/test');

test('shows login page on initial load', async ({ page }) => {
  await page.goto('/');
  await expect(page.getByText('Sign in with GitHub')).toBeVisible();
  await expect(page.getByText('View PR diffs with difftastic')).toBeVisible();
});

test('has page title', async ({ page }) => {
  await page.goto('/');
  await expect(page).toHaveTitle('Difft PR');
});

test('login button exists and is clickable', async ({ page }) => {
  await page.goto('/');
  const loginButton = page.getByRole('button', { name: 'Sign in with GitHub' });
  await expect(loginButton).toBeVisible();
  await expect(loginButton).toBeEnabled();
});

test('header shows app name', async ({ page }) => {
  await page.goto('/');
  await expect(page.locator('.header h1')).toHaveText('Difft PR');
});

test('no user info shown when not logged in', async ({ page }) => {
  await page.goto('/');
  await expect(page.locator('.user-info')).toHaveCount(0);
});
