import type { z } from 'zod';

const formatError = (error: z.ZodError) => {
  const [firstError, ...otherErrors] = error.errors;
  const firstErrorPath = firstError.path.join('.');

  return `'${firstErrorPath}: ${firstError.message}'${
    otherErrors.length > 0 ? ` and ${otherErrors.length} other(s)` : ''
  }`;
};

export class GenericParsingError extends Error {
  constructor(public readonly key: string, public readonly innerError?: z.ZodError) {
    super(`Failed to parse raw shape ${key} with error: ${innerError ? formatError(innerError) : 'Unknown'}`);
  }
}
