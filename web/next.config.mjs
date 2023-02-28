//@ts-check

!process.env.SKIP_ENV_VALIDATION && (await import('./env.mjs'));

/** @type {import('next').NextConfig} */
const nextConfig = {
  reactStrictMode: true,
  output: process.env.NEXT_ENV_DOCKER ? 'standalone' : undefined,
  redirects: async () => {
    return [
      {
        source: '/mainnet/MIP1',
        destination: '/proposal/1',
        permanent: true,
      },
    ];
  },
  modularizeImports: {
    '@mui/icons-material': {
      transform: '@mui/icons-material/{{member}}',
    },
    '@mui/material': {
      transform: '@mui/material/{{member}}',
    },
  },
};

export default nextConfig;
