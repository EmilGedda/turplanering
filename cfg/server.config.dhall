let DBConfig : Type =
    { dbAddr    : Text
    , dbPort    : Natural
    , username  : Text
    , password  : Text
    , database  : Text
    }

let HTTPConfig : Type =
    { httpPort : Natural
    , httpAddr : Text
    }

let LogLevel = < Trace
               | Debug
               | Info
               | Warning
               | Error
               | Fatal
               | Silent
               >

let LogStyle = < Auto | Pretty | Structured >

let LoggerConfig : Type =
    { level : LogLevel
    , style : LogStyle
    }

let Environment = < Production | Development >

let AppConfig : Type =
    { db        : DBConfig
    , http      : HTTPConfig
    , logger    : LoggerConfig
    , environ   : Environment
    }

let devConfig : AppConfig =
    { db =
        { dbAddr   = env:TUR_DB_ADDRESS  as Text ? "localhost"
        , dbPort   = env:TUR_DB_PORT             ? 5432
        , username = env:TUR_DB_USER     as Text ? "user"
        , password = env:TUR_DB_PASSWORD as Text ? "password"
        , database = env:TUR_DB_NAME     as Text ? "turplanering"
        }
    , http =
        { httpPort = env:TUR_HTTP_PORT ? 4000
        , httpAddr = env:TUR_HTTP_ADDR ? "localhost"
        }
    , logger =
        { level = env:TUR_LOGGER_LEVEL ? LogLevel.Trace
        , style = env:TUR_LOGGER_STYLE ? LogStyle.Auto
        }
    , environ = env:TUR_ENV ? Environment.Development
    }
in devConfig
