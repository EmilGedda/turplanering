let DBConfig : Type =
    { address   : Text
    , port      : Natural
    , username  : Text
    , password  : Text
    , database  : Text
    }

let GQLConfig : Type =
    { port : Natural }

let LogLevel = < Trace
               | Debug
               | Info
               | Warning
               | Error
               >

let LogStyle = < Auto | Pretty | Structured >

let LoggerConfig : Type =
    { level : LogLevel
    , style : LogStyle
    }

let Environment = < Production | Development >

let AppConfig : Type =
    { db        : DBConfig
    , gql       : GQLConfig
    , logger    : LoggerConfig
    , environ   : Environment
    }

let devConfig : AppConfig =
    { db =
        { address  = env:TUR_DB_ADDRESS  as Text ? "localhost"
        , port     = env:TUR_DB_PORT             ? 5432
        , username = env:TUR_DB_USER     as Text ? "user"
        , password = env:TUR_DB_PASSWORD as Text ? "password"
        , database = env:TUR_DB_NAME     as Text ? "turplanering"
        }
    , gql = { port = env:TUR_GQL_PORT ? 4000 }
    , logger =
        { level = env:TUR_LOGGER_LEVEL ? LogLevel.Trace
        , style = env:TUR_LOGGER_STYLE ? LogStyle.Auto
        }
    , environ = env:TUR_ENV ? Environment.Development
    }
in devConfig
