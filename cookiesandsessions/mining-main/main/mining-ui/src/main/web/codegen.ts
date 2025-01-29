
import type { CodegenConfig } from '@graphql-codegen/cli';

const config: CodegenConfig = {
  overwrite: true,
  schema: "schema.graphql",
  documents: "./src/app/graphql/queries/*.graphql",
  generates: {
    "./src/app/graphql/generated/generated.ts": {
      plugins: ['typescript', 'typescript-operations',"typescript-apollo-angular"]
    }
  },
  verbose: true
};

export default config;
