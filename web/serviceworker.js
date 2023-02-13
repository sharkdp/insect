self.addEventListener('install', () => self.skipWaiting());
self.addEventListener('activate', (event) => event.waitUntil(self.clients.claim()));

self.addEventListener('fetch', (event) => {
  if (event.request.method !== 'GET') {
    return;
  }

  event.respondWith(networkOrCache(event));
});

async function putInCache(event, response) {
  const cache = await caches.open('insect-cache');
  await cache.put(event.request, response);
}

async function networkOrCache(event) {
  try {
    const response = await fetch(event.request, {
      cache: 'no-cache'
    });

    if (response.ok) {
      event.waitUntil(putInCache(event, response));

      return response.clone();
    }

    return response;
  } catch (e) {
    const cache = await caches.open('insect-cache');
    const matching = await cache.match(event.request);

    return matching || Promise.reject(e);
  }
}
