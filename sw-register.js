// Only register the service worker in production (avoids MIME/404 issues on local dev)
if ('serviceWorker' in navigator && location.hostname !== 'localhost') {
  window.addEventListener('load', () => {
    navigator.serviceWorker.register(new URL('./sw.js', import.meta.url).href).catch(() => {});
  });
}

/** Unregister all service workers and reload so the next load fetches fresh assets. Called from the app (Overview screen). */
window.refreshApp = function () {
  if (!navigator.serviceWorker) {
    location.reload();
    return;
  }
  navigator.serviceWorker.getRegistrations().then(function (regs) {
    return Promise.all(regs.map(function (r) { return r.unregister(); }));
  }).then(function () {
    location.reload();
  });
};
