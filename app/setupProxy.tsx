export {}

const { createProxyMiddleware } = require('http-proxy-middleware');

module.exports = function(app : any) {
  app.use(
    '/api',
    createProxyMiddleware({
      target: 'http://35.203.38.140:8080/votes',
      changeOrigin: true,
    })
  );
};