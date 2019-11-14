-- Copyright (c) 2019 The DAML Authors. All rights reserved.
-- SPDX-License-Identifier: Apache-2.0
module Main (main) where

import qualified DA.Daml.LF.Proto3.Archive as Archive
import qualified DA.Daml.LF.Reader as DAR
import qualified DA.Pretty
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.NameMap as NM
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
    dup :: a -> (a, a)
    dup x = (x, x)
    genModule :: Module -> T.Text
    genModule mod = T.unlines $
        ["// Generated from " <> T.intercalate "/" (unModuleName (moduleName mod)) <> ".daml"
        ,"import * as daml from '../../ledger/types';"
        ,"import { object } from '@mojotech/json-type-validation';"
        ]
        ++ concat
            [ [""] ++ def' ++ ser
            | def <- NM.toList (moduleDataTypes mod)
            , let (def', ser) = genDefDataType (moduleName mod) def
            ]

    genDefDataType :: ModuleName -> DefDataType -> ([T.Text], [T.Text])
    genDefDataType curModName def
        | not (getIsSerializable (dataSerializable def)) = ([], [])
        | otherwise = case unTypeConName (dataTypeCon def) of
            [] -> error "IMPOSSIBLE: empty type constructor name"
            _:_:_ -> error "TODO(MH): multi-part type constructor names"
            [conName] -> case dataCons def of
                DataVariant{} -> error "TODO(MH): variant types"
                DataEnum{} -> error "TODO(MH): enum types"
                DataRecord fields ->
                    let paramNames = map (unTypeVarName . fst) (dataParams def)
                        typeParams
                          | null paramNames = ""
                          | otherwise = "<" <> T.intercalate ", " paramNames <> ">"
                        serParam paramName = paramName <> ": daml.Serializable<" <> paramName <> ">"
                        serHeader
                          | null paramNames = ": daml.Serializable<" <> conName <> "> ="
                          | otherwise = " = " <> typeParams <> "(" <> T.intercalate ", " (map serParam paramNames) <> "): daml.Serializable<" <> conName <> typeParams <> "> =>"
                        (fieldNames, fieldTypesLf) = unzip [(unFieldName x, t) | (x, t) <- fields]
                        (fieldTypesTs, fieldSers) = unzip (map (genType curModName) fieldTypesLf)
                    in
                    ( ["export type " <> conName <> typeParams <> " = {"] ++
                      ["  " <> x <> ": " <> t <> ";" | (x, t) <- zip fieldNames fieldTypesTs] ++
                      ["};"]
                    , ["export const " <> conName <> serHeader <> " ({"
                      ,"  decoder: () => object({"
                      ] ++
                      ["    " <> x <> ": " <> ser <> ".decoder()," | (x, ser) <- zip fieldNames fieldSers] ++
                      ["  }),"
                      ,"});"
                      ]
                    )


    genType :: ModuleName -> Type -> (T.Text, T.Text)
    genType curModName = go
      where
        go = \case
            TVar v -> dup (unTypeVarName v)
            TUnit -> ("{}", "daml.Unit")
            TBool -> ("boolean", "daml.Bool")
            TInt64 -> dup "daml.Int"
            TDecimal -> dup "daml.Decimal"
            TNumeric _ -> dup "daml.Numeric"  -- TODO(MH): Figure out what to do with the scale.
            TText -> ("string", "daml.Text")
            TTimestamp -> dup "daml.Time"
            TParty -> dup "daml.Party"
            TDate -> dup "daml.Date"
            TList t ->
                let (t', ser) = go t
                in
                (t' <> "[]", "daml.List(" <> ser <> ")")
            TOptional (TOptional _) -> error "TODO(MH): nested optionals"
            TOptional t ->
                let (t', ser) = go t
                in
                ("(" <> t' <> " | null)", "daml.Optional(" <> ser <> ")")
            TMap t  ->
                let (t', ser) = go t
                in
                ("{ [key: string]: " <> t' <> " }", "daml.TextMap(" <> ser <> ")")
            TUpdate _ -> error "IMPOSSIBLE: Update not serializable"
            TScenario _ -> error "IMPOSSIBLE: Scenario not serializable"
            TContractId t ->
                let (t', ser) = go t
                in
                ("daml.ContractId<" <> t' <> ">", "daml.ContractId(" <> ser <> ")")
            TConApp con ts ->
                let (con', ser) = genTypeCon curModName con
                    (ts', sers) = unzip (map go ts)
                in
                if null ts
                    then (con', ser)
                    else
                        ( con' <> "<" <> T.intercalate ", " ts' <> ">"
                        , ser <> "(" <> T.intercalate ", " sers <> ")"
                        )
            TCon _ -> error "IMPOSSIBLE: lonely type constructor"
            t@TApp{} -> error $ "IMPOSSIBLE: type application not serializable - " <> DA.Pretty.renderPretty t
            TBuiltin t -> error $ "IMPOSSIBLE: partially applied primitive type not serializable - " <> DA.Pretty.renderPretty t
            TForall{} -> error "IMPOSSIBLE: universally quantified type not serializable"
            TTuple{} -> error "IMPOSSIBLE: structur record not serializable"
            TNat{} -> error "IMPOSSIBLE: standalone type level natural not serializable"

    genTypeCon :: ModuleName -> Qualified TypeConName -> (T.Text, T.Text)
    genTypeCon curModName (Qualified pkgRef modName conParts) = case pkgRef of
        PRImport _ -> error "TODO(MH): package imports"
        PRSelf -> case unTypeConName conParts of
            [] -> error "IMPOSSIBLE: empty type constructor name"
            _:_:_ -> error "TODO(MH): multi-part type constructor names"
            [conName]
                | modName == curModName -> dup conName
                | otherwise -> dup (T.intercalate "." (unModuleName modName) <> "." <> conName)

