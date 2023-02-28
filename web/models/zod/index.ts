import type { z } from 'zod';

import { GenericParsingError } from './error';
import { getCoreApiInfoResponseSchema, getProposalResultsSchema, getProposalSchema } from './schema';

class ZodParser<T, K extends z.ZodSchema<T>> {
  schema: K;

  constructor(schema: K) {
    this.schema = schema;
  }

  parse(key: string, raw: T): z.infer<K> {
    const result = this.schema.safeParse(raw);

    if (!result.success) {
      throw new GenericParsingError(key, result.error);
    }

    return result.data;
  }
}

const parserFactory = <T, K extends z.ZodSchema<T>>(schema: K) => new ZodParser(schema);

export const ProposalParser = parserFactory(getProposalSchema);
export type ProposalParserOutcome = ReturnType<typeof ProposalParser.parse>;

export const ProposalResultsParser = parserFactory(getProposalResultsSchema);
export type ProposalResultsParserOutcome = ReturnType<typeof ProposalResultsParser.parse>;

export const CoreApiInfoParser = parserFactory(getCoreApiInfoResponseSchema);
export type CoreApiInfoParserOutcome = ReturnType<typeof CoreApiInfoParser.parse>;
