// Only register the service worker in production (avoids MIME/404 issues on local dev)
if ('serviceWorker' in navigator && location.hostname !== 'localhost') {
  window.addEventListener('load', () => {
    navigator.serviceWorker.register(new URL('./sw.js', import.meta.url).href).catch(() => {});
  });
}
