# sample-app

## Build

```bash
stack install --pedantic
```

## Run app

```bash
DB_CONN_STRING=postgresql://$USER_NAME:$DB_PASSWORD@localhost:5432/$DB_NAME stack exec sample-app
```

## Steps to cause migration error

1. そのままビルド
2. 実行
3. `src/TonaApp/Db/EntityDefs.hs`の`data1`を削除してビルド
4. 実行
