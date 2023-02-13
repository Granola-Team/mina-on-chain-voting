import type { NextRequest } from 'next/server';
import { NextResponse } from 'next/server';

/**
 * Middleware that runs on every request.
 * Sets default theme cookie.
 */
export function middleware(req: NextRequest) {
  const res = NextResponse.next();

  if (!req.cookies.has('theme')) {
    res.cookies.set({ name: 'theme', value: 'dark', sameSite: 'strict' });
  }

  return res;
}

export const config = {
  matcher: [
    /*
     * Match all request paths except for the ones starting with:
     * - api (API routes)
     * - _next/static (static files)
     * - favicon.ico (favicon file)
     */
    '/((?!api|_next/static|favicon.ico).*)',
  ],
};
