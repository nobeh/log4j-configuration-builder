package org.apache.logging.log4j.core.util;


import java.nio.file.Paths;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class Examples {

  /**
   * A configuration on console with DEBUG level
   */
  static void exampleConfiguration1() {
    ConfigurationBuilder.Builder builder = ConfigurationBuilder.newConfiguration();
    builder.setConfigurationName("my-config-1");
    builder.disableAsyncLoggers();
    builder.setLevel("DEBUG");
    builder.addAppender("root-appender", null, null);
    builder.setAppenderPatternLayout("root-appender", "[%d] [%level] [%thread] %msg%n");
    builder.addRootAppender("root-appender");
    builder.configure();

    Logger logger = LogManager.getLogger(Examples.class);
    logger.debug("Logging example 1 ...");
  }

  /**
   * A configuration on file with DEBUG level
   */
  static void exampleConfiguration2() {
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
  }

  /**
   * A configuration on file with DEBUG level with named loggers
   */
  static void exampleConfiguration3() {
    ConfigurationBuilder.Builder builder = ConfigurationBuilder.newConfiguration();
    builder.setConfigurationName("my-config-3");
    builder.disableAsyncLoggers();
    builder.setLevel("DEBUG");

    builder.setLogDirectory(Paths.get("/tmp/myapp"));
    builder.addAppender("file-appender", "myapp.log", "myapp.log-%d{yyyy-MM-dd}");
    builder.setAppenderPatternLayout("file-appender",
        "[%d] [%level] [%thread] %msg (%logger{1})%n");

    builder.addAppender("console-appender", null, null);
    builder.setAppenderPatternLayout("console-appender",
        "[%d] [%level] [%thread] %msg (%logger{1})%n");

    builder.addLogger("custom-logger", "file-appender", "console-appender");
    builder.addRootAppender("file-appender").addRootAppender("console-appender");
    builder.configure();

    Logger logger = LogManager.getLogger("custom-logger");
    logger.debug("Logging example 3 ...");

  }

  /**
   * A default configuration with syslog integration
   */
  static void exampleConfiguration4Syslog() {
    ConfigurationBuilder.Builder builder = ConfigurationBuilder.newConfiguration();
    builder.setConfigurationName("my-config-syslog");
    builder.disableAsyncLoggers();
    builder.setLevel("DEBUG");
    builder.addAppender("root-appender", null, null);
    builder.addSyslogAppender("root-syslog", "10.100.102.89", 514, "UDP", "test-app", "USER", 1000,
        "nobeh");
    builder.setAppenderPatternLayout("root-appender", "[%d] [%level] [%thread] %msg%n");
    builder.addRootAppender("root-appender");
    builder.configure();

    Logger logger = LogManager.getLogger(Examples.class);
    logger.debug("Debug logging example 4 + syslog ...");
    logger.info("Info logging example 4 + syslog ...");
    logger.error("Error logging example 4 + syslog ...");
  }

  public static void main(String[] args) throws Exception {
    exampleConfiguration4Syslog();
    Thread.sleep(5000);
  }

}
