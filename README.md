# Apache Log4j 2 Configuration Builder

Apache Log4j 2 provides a simple API to be configure a logging context having a `log4j.xml` with [Configurator][1]. This module provides a similar way to build an Apache Log4j2 logging context with no need to have a `log4j.xml`. It implements Apache Log4j 2 [`Builder`][2] API and configures a logging context. 

An example:

```java
import org.apache.logging.log4j.core.util.ConfigurationBuilder;
import org.apache.logging.log4j.core.util.ConfigurationBuilder.Builder;

public static void configureLog4j() {
	String appName = "myapp";
	Path logDirectory = Paths.get("/path/to/myapp/logs")
    String rootAppenderName = appName + "-appender";
    String extraAppenderName = appName + "-extra" + "-appender";
    Builder builder = ConfigurationBuilder.newConfiguration();
    builder.setConfigurationName(appName + "-logging");
    builder.setLogDirectory(logDirectory);
    builder.addAppender(rootAppenderName, 
    	"myapp.log", "myapp.log.%d{yyyy-MM-dd}");
    builder.setAppenderPatternLayout(rootAppenderName, 
    	"[%d] [%level] [%thread] %msg (%logger{1}:%L)%n%throwable");
    builder.addRootAppender(rootAppenderName);
    builder.addAppender(extraAppenderName, 
    	"myapp-extra.log", "myapp-extra.log.%d{yyyy-MM-dd}");
    builder.setAppenderPatternLayout(extraAppenderName, 
    	"%d{ISO8601} %level %msg%n");
    builder.addLogger("myapp-extra-logger", extraAppenderName);
    builder.configure();
}
```

The current implementation:

* Uses asynchronous loggers. Can be disabled by `builder.disableAsyncLoggers()`.
* Disable JMX by default. Can be enabled by `builder.enableJMX()`.
* Does not support `syslog` configuration yet.
* Installs a shutdown hook to ensure that logging context stops.
* Creates a *daily* appender with a rollover period of 30 days with GZIP compression.
* If appender file name is `null`, it is concluded as a *console* appender.

[1]: https://logging.apache.org/log4j/2.x/log4j-core/apidocs/org/apache/logging/log4j/core/config/Configurator.html
[2]: https://logging.apache.org/log4j/2.x/log4j-core/apidocs/org/apache/logging/log4j/core/util/Builder.html
