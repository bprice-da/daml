-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
module Main (main) where

import qualified DA.Daml.LF.Proto3.Archive as Archive
import qualified DA.Daml.LF.Reader as DAR
import qualified DA.Pretty
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified "zip-archive" Codec.Archive.Zip as Zip

import Control.Monad
import DA.Daml.LF.Ast
import Options.Applicative

data Options = Options
    { inputDar :: FilePath
    , outputDir :: FilePath
    }

optionsParser :: Parser Options
optionsParser = Options
    <$> argument str
        (  metavar "DAR-FILE"
        <> help "DAR file to generate TypeScript bindings for"
        )
    <*> strOption
        (  short 'o'
        <> metavar "DIR"
        <> help "Output directory for the generated TypeScript files"
        )

optionsParserInfo :: ParserInfo Options
optionsParserInfo = info (optionsParser <**> helper)
    (  fullDesc
    <> progDesc "Generate TypeScript bindings from a DAR"
    )

main :: IO ()
main = do
    Options{inputDar, outputDir = _} <- execParser optionsParserInfo
    dar <- B.readFile inputDar
    dalfs <- either fail pure $ DAR.readDalfs $ Zip.toArchive $ BSL.fromStrict dar
    let bytes = BSL.toStrict $ DAR.mainDalf dalfs
    (pkgId, pkg) <- either (fail . show)  pure $ Archive.decodeArchive Archive.DecodeAsMain bytes
    daml2ts pkgId pkg

daml2ts :: PackageId -> Package -> IO ()
daml2ts _pkgId pkg = do
    forM_ (packageModules pkg) $ \mod -> do
        T.putStrLn (genModule mod)
  where
    genModule :: Module -> T.Text
    genModule mod = T.unlines $
        ["// " <> T.intercalate "/" (unModuleName (moduleName mod))]
        ++ concatMap (genDefDataType (moduleName mod)) (moduleDataTypes mod)

    genDefDataType :: ModuleName -> DefDataType -> [T.Text]
    genDefDataType curModName def
        | not (getIsSerializable (dataSerializable def)) = []
        | otherwise = case unTypeConName (dataTypeCon def) of
            [] -> error "IMPOSSIBLE: empty type constructor name"
            _:_:_ -> error "TODO(MH): multi-part type constructor names"
            [conName] -> case dataCons def of
                DataVariant{} -> error "TODO(MH): variant types"
                DataEnum{} -> error "TODO(MH): enum types"
                DataRecord fields ->
                    let params
                          | null (dataParams def) = ""
                          | otherwise = "<" <> T.intercalate ", " (map (unTypeVarName . fst) (dataParams def)) <> ">"
                    in
                    ["type " <> conName <> params <> " = {"]
                    ++ ["  " <> unFieldName x <> ": " <> genType curModName t <> ";" | (x, t) <- fields]
                    ++ ["}"]


    genType :: ModuleName -> Type -> T.Text
    genType curModName = go
      where
        go = \case
            TVar v -> unTypeVarName v
            TUnit -> "{}"
            TBool -> "boolean"
            TInt64 -> "daml.Int"
            TDecimal -> "daml.Decimal"
            TNumeric _ -> "daml.Numeric"  -- TODO(MH): Figure out what to do with the scale.
            TText -> "string"
            TTimestamp -> "daml.Time"
            TParty -> "daml.Party"
            TDate -> "daml.Date"
            TList t -> go t <> "[]"
            TOptional (TOptional _) -> error "TODO(MH): nested optionals"
            TOptional t -> "(" <> go t <> "| null)"
            TMap t  -> "{ [key: string]: " <> go t <> " }"
            TUpdate _ -> error "IMPOSSIBLE: Update not serializable"
            TScenario _ -> error "IMPOSSIBLE: Scenario not serializable"
            TContractId t -> "daml.ContractId<" <> go t <> ">"
            TConApp con ts ->
                let ts' | null ts = ""
                        | otherwise = "<" <> T.intercalate ", " (map go ts) <> ">"
                in
                genTypeCon curModName con <> ts'
            TCon _ -> error "IMPOSSIBLE: lonely type constructor"
            t@TApp{} -> error $ "IMPOSSIBLE: type application not serializable - " <> DA.Pretty.renderPretty t
            TBuiltin t -> error "IMPOSSIBLE: partially applied primitive type not serializable - " <> DA.Pretty.renderPretty t
            TForall{} -> error "IMPOSSIBLE: universally quantified type not serializable"
            TTuple{} -> error "IMPOSSIBLE: structur record not serializable"
            TNat{} -> error "IMPOSSIBLE: standalone type level natural not serializable"

    genTypeCon :: ModuleName -> Qualified TypeConName -> T.Text
    genTypeCon curModName (Qualified pkgRef modName conParts) = case pkgRef of
        PRImport _ -> error "TODO(MH): package imports"
        PRSelf -> case unTypeConName conParts of
            [] -> error "IMPOSSIBLE: empty type constructor name"
            _:_:_ -> error "TODO(MH): multi-part type constructor names"
            [conName]
                | modName == curModName -> conName
                | otherwise -> T.intercalate "." (unModuleName modName) <> "." <> conName

