import Data.ProtoLens.Setup
main = do
    -- TODO: Move the generated files from BuildInfo.autogen... to $ROOT/.protobuf
    -- until pblens moves to module-specific autogen directories
    putStrLn "**** Protobuf here"
    -- defaultMainGeneratingProtos "../protobuf/com.acme"