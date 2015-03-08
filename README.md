# Apache Log4j 2 Configuration Builder

Apache Log4j 2 provides a simple API to be configure a logging context having a `log4j.xml` with [Configurator][1]. This module provides a similar way to:

* Eliminate the need to have a `log4j.xml` file for the configuration
* Simplify the chain of API calls necessary to prepare an Apache Log4j logging context

by implementating Apache Log4j 2 [`Builder`][2] API.

## Examples

### A configuration on console with DEBUG level

```java
ConfigurationBuilder.Builder builder = ConfigurationBuilder.newConfiguration();
builder.setConfigurationName("my-config");
builder.disableAsyncLoggers();
builder.setLevel("DEBUG");
builder.addAppender("root-appender", null, null);
builder.setAppenderPatternLayout("root-appender", "[%d] [%level] [%thread] %msg%n");
builder.addRootAppender("root-appender");
builder.configure();

Logger logger = LogManager.getLogger(Examples.class);
logger.debug("Logging example ...");
```

### A configuration on file with DEBUG level

```java
ConfigurationBuilder.Builder builder = ConfigurationBuilder.newConfiguration();
builder.setConfigurationName("my-config-2");
builder.disableAsyncLoggers();
builder.setLevel("DEBUG");

builder.setLogDirectory(Paths.get("/tmp/myapp"));
builder.addAppender("default-appender", "myapp.log", "myapp.log-%d{yyyy-MM-dd}");
builder.setAppenderPatternLayout("default-appender", "[%d] [%level] [%thread] %msg%n");

builder.addRootAppender("default-appender");
builder.configure();

Logger logger = LogManager.getLogger(Examples.class);
logger.debug("Logging example 2 ...");
```

### A configuration on file with DEBUG level with named loggers

```java
ConfigurationBuilder.Builder builder = ConfigurationBuilder.newConfiguration();
builder.setConfigurationName("my-config-3");
builder.disableAsyncLoggers();
builder.setLevel("DEBUG");

builder.setLogDirectory(Paths.get("/tmp/myapp"));
builder.addAppender("file-appender", "myapp.log", "myapp.log-%d{yyyy-MM-dd}");
builder
    .setAppenderPatternLayout("file-appender", "[%d] [%level] [%thread] %msg (%logger{1})%n");

builder.addAppender("console-appender", null, null);
builder.setAppenderPatternLayout("console-appender",
    "[%d] [%level] [%thread] %msg (%logger{1})%n");

builder.addLogger("custom-logger", "file-appender", "console-appender");
builder.addRootAppender("file-appender").addRootAppender("console-appender");
builder.configure();

Logger logger = LogManager.getLogger("custom-logger");
logger.debug("Logging example 3 ...");
```

## Limitations

The current implementation:

* Does not support `syslog` appenders.
* Does not support file appenders in simple mode with no rollover strategy.
* Does not support database appenders.
* Installs a shutdown hook to ensure that logging context stops.

[1]: https://logging.apache.org/log4j/2.x/log4j-core/apidocs/org/apache/logging/log4j/core/config/Configurator.html
[2]: https://logging.apache.org/log4j/2.x/log4j-core/apidocs/org/apache/logging/log4j/core/util/Builder.html
