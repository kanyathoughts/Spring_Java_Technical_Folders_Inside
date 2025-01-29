# How to build

1. Install [node.js](https://nodejs.org/en/download/)
2. Recommended: install `lerna` command line tool (otherwise prefix lerna with `npx` in the examples below)
```
npm install -g lerna
```
3. Setup and build the project
```
npm install
lerna bootstrap
npm run build-all
```
This will build the binaries. They will be placed in `packages/mining-cli/bin/*`

# How to hack

1. Open the `packages/` folder in IDE of your choice (VS Code)
2. Edit to your heart's content
3. Run tests (there aren't a lot currently) with
```
npm run test
```
4. Rebuild with
```
cd packages/mining-cli
npm run build
```
5. Run directly from the build directory via
```
node dist/
```
6. Rebuild the binaries with
```
npm run pkg
```
7. Binaries are put in `bin/`

## How to regenerate mining-api-client

When the API of the server changes or new endpoints are introduced, it is necessary to rebuild the api-client. To do this, follow these steps:

1. Install [swagger-codegen](https://github.com/swagger-api/swagger-codegen)
2. Start mining-api-server on localhost (make sure you are using the latest nightly build or TRUNK snapshot, so the latest version of the API is generated!)
3. Regenerate the api-client with
```
cd packages/mining-api-client
swagger-codegen generate -i http://localhost:8080/v2/api-docs -l typescript-axios
```
4. Manually fix compile errors (I have no idea why swagger-codegen is so bad).
    1. In `info-controller-api.ts` it generates
    ```
    AxiosPromise<{ [key, string]: string;}>>
    ```
    this is invalid syntax. Change it to:
    ```
    AxiosPromise<{ [key: string]: string;}>>
    ```
    This also occurs in a few other places.
    2. In `getJobInformations()` in `job-controller-api.ts` the name of the `query` parameter collides with auto-generated variable names. Therefore, rename `query` to something else, e.g. `query_` (or just revert the change)
    3. Also in `job-controller-api.ts` the generated code for `submitJobExtensionV2()` is complete garbage:
        - it doesn't pass in the query parameters correctly, instead somehow wrapping them into a "parameters" argument
        - when the body is _not_ a string, it tries to `JSON.stringify()` it -- that doesn't go well when trying to send binary data
        - it types `parameters` as `ModelObject` and even generates an import for this non-existing type. The correct type is `{ [key: string]: Array<string>;}`
    4. A random `null` appears in the (questionable) type definition in `mapstring-mining-data-point-definition.ts`:
    ```
    export interface MapstringMiningDataPointDefinition extends null<String, MiningDataPointDefinition>
    ```
    change it to (and add the missing import for `MiningDataPointDefinition`):
    ```
    export interface MapstringMiningDataPointDefinition extends Map<String, MiningDataPointDefinition>
    ```
    The same issue occurs in a couple of other `Map` definitions.

# How to use

## Project setup

1. Go to your project containing the source you want to use for Discovery / Mining

2. Create a file named `.mining-cli-project`. It's a JSON file containing the project configuration. The minimal configuration file needs to contain an empty JSON object:
```json
{}
```
*TODO:* mining-cli should be able to handle empty/missing config file. For now you need to create it manually and put in the above content.

3. OPTIONAL: Configure the server url and credentials by editing `.mining-cli-project`.
---
⚠️ As of now mining-cli has no support for keycloak. So only the "legacy auth" an "no auth" modes are supported

---
```json
{
    "server": {
        "url": "http://my-server:8080",
        "accessToken": "0815-4711-abcd-efgh"
    }
}
```

4. Select a client with `mining-cli client select <clientId>`. You can list clients with `mining-cli clients list` and create a client with `mining-cli client create`.

5. Select a client with `mining-cli project select <projectId>`. You can list clients with `mining-cli projects list` and create a client with `mining-cli projects create`.

Now you are ready to run any mining/discovery command. See `mining-cli --help`

## How to run Discovery

1. Upload folder(s) with sources. You can specify multiple folders on the command line:
```
mining-cli discovery upload-source src src2
```

2. Run Discover Code. Note that mining-cli currently does not download the "result" of Discover Code - i.e. your local source will remain unchanged:
```
mining-cli discovery discover-code
```

3. Run Discover Metrics:
```
mining-cli discovery discover-metrics
```
4. OPTIONAL: Download the result as Excel or CSV:
```
mining-cli discovery export-excel
mining-cli discovery export-csv
```
