## Biofinder example

### Some signatures for basic functions

```haskell

putInStore :: Storable a => Flow a ContentHash
getFromStore :: Storable a => Flow ContentHash a

data DockerBind =
    -- | Single input, gets mounted to '/input' on the image
    SingleInput ContentHash
    -- | Multiple inputs; each gets mounted into a subdirectory under
    --   '/input' on the image, depending on its
  | MultiInput [(ContentHash, FilePath)]

instance ContentHashable DockerBind

data DockerConfig = DockerConfig {
    image :: T.Text
  , inputBinds :: DockerBind
  , cmdToRun :: FilePath
  , cmdArgs :: [T.Text]
}

instance ContentHashable DockerConfig

class ToDockerConfig a where
  toDockerConfig :: a -> DockerConfig

instance ToDockerConfig DockerConfig where
  toDockerConfig = id

dockerFlow :: ToDockerConfig a => Flow a ContentHash
```

### Model training

This is an example of a biofinder workflow which might run. It performs the
following actions:

1. Loads data from a Postgres instance
2. Puts these data into the contenthashable store
3. Invokes a docker container to fit PCA on the given data


```haskell

data FetchFromPostgresConfig = FetchFromPostgresConfig {
    db_host :: T.Text
  , db_port :: Int
  , db_user :: T.Text
  , db_password :: T.Text
  , db_schema :: T.Text
  , db_query :: T.Text
}

instance ContentHashable PostgresConfig

fetchFromPostgres :: Flow FetchFromPostgresConfig ContentHash
fetchFromPosgres = proc config -> do
  results <- (effectChoice $ do
    ... connect to postgres, pull data) -< config
  putInStore -< results

fitPCAModel :: Flow ContentHash ContentHash
fitPCAModel = proc input -> do
  dockerFlow -< DockerConfig
                  "pfizer/transformPCA"
                  (SingleInput input)
                  "/usr/bin/python"
                  ["..."]



data TrainPCAParams = TrainPCAParams {
    db_config :: FetchFromPostgresConfig
}

instance ContentHashable TrainPCAParams

trainPCA :: Flow TrainPCAParams ContentHash
trainPCA = proc config -> do
  input <- fetchFromPostgres -< db_config config
  fitPCAModel -< input

```
### Model projection

Projection performs the following:

1. Takes the name of a table from the UI
2. Loads the relevant data from that table into the store
3. Runs a transform pipeline to map to a model
4. Loads the ouput ready for plotting

```haskell

projectOnto :: Flow ContentHash ContentHash
projectOnto = proc input -> do
  dockerFlow -< DockerConfig
                  "pfizer/trainPCA"
                  (SingleInput input)
                  "/usr/bin/python"
                  ["..."]

transformPCA :: Flow TableName (Matrix Int Int)
transformPCA = proc tableName -> do
  input <- (sqlQuery $ \tableName -> TL.toStrict
              $ format "select * from {}" ( Only tableName )
           )
        -< tableName
  inputPath <- putInStore -< input
  resultPath <- projectOnto -< inputPath
  results <- getFromStore -< resultPath

```
