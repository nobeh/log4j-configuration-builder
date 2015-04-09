package org.apache.logging.log4j.core.util;

import java.net.URI;
import java.nio.charset.Charset;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.zip.Deflater;

import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.core.Appender;
import org.apache.logging.log4j.core.Filter;
import org.apache.logging.log4j.core.LoggerContext;
import org.apache.logging.log4j.core.appender.ConsoleAppender;
import org.apache.logging.log4j.core.appender.ConsoleAppender.Target;
import org.apache.logging.log4j.core.appender.FileAppender;
import org.apache.logging.log4j.core.appender.RollingFileAppender;
import org.apache.logging.log4j.core.appender.SyslogAppender;
import org.apache.logging.log4j.core.appender.rolling.DefaultRolloverStrategy;
import org.apache.logging.log4j.core.appender.rolling.RolloverStrategy;
import org.apache.logging.log4j.core.appender.rolling.TimeBasedTriggeringPolicy;
import org.apache.logging.log4j.core.appender.rolling.TriggeringPolicy;
import org.apache.logging.log4j.core.async.AsyncLogger;
import org.apache.logging.log4j.core.async.AsyncLoggerConfig;
import org.apache.logging.log4j.core.async.AsyncLoggerContext;
import org.apache.logging.log4j.core.config.AbstractConfiguration;
import org.apache.logging.log4j.core.config.AppenderRef;
import org.apache.logging.log4j.core.config.Configuration;
import org.apache.logging.log4j.core.config.ConfigurationFactory;
import org.apache.logging.log4j.core.config.ConfigurationSource;
import org.apache.logging.log4j.core.config.DefaultConfiguration;
import org.apache.logging.log4j.core.config.LoggerConfig;
import org.apache.logging.log4j.core.config.Property;
import org.apache.logging.log4j.core.layout.LoggerFields;
import org.apache.logging.log4j.core.layout.PatternLayout;
import org.apache.logging.log4j.core.net.Facility;
import org.apache.logging.log4j.core.net.ssl.SslConfiguration;

/**
 * A builder class to build an embedded in-memory equivalent of
 * <code>log4j.xml</code>for Apache Log4j2.
 * 
 * @author Behrooz Nobakht
 */
public class ConfigurationBuilder {

  /**
   * Property to control if JMX server is enabled.
   * 
   * @see {@link org.apache.logging.log4j.core.jmx.Server}
   * @see #enableJMX()
   */
  private static final String LOG4J_DISABLE_JMX = "log4j2.disable.jmx";

  /**
   * The key with which Apache Log4j loads the selector class.
   * 
   * @see <a href=
   *      "http://logging.apache.org/log4j/2.0/manual/async.html">
   *      Async Loggers</a>
   */
  private static final String LOG4J_ASYNC_LOGGERS = "Log4jContextSelector";

  static {
    System.setProperty(LOG4J_DISABLE_JMX, Boolean.TRUE.toString());
    System.setProperty(LOG4J_ASYNC_LOGGERS,
        org.apache.logging.log4j.core.async.AsyncLoggerContextSelector.class.getName());
  }

  /**
   * A value object to hold the configuration.
   */
  private static final class Spec {

    private final String name;
    private final boolean asyncLoggers;
    private final Level level;
    private final Path directory;
    private final Map<String, List<String>> loggers;
    private final Map<String, String> appenders;
    private final Map<String, String> appenderFileNamePatterns;
    private final Map<String, String> appenderHeaders;
    private final Map<String, String> appenderFooters;
    private final Map<String, String> appenderPatterns;
    private final Set<String> rootAppenders;
    private final Map<String, SyslogSpec> syslogAppenders;

    /**
     * Logging configuration specification constructor.
     * 
     * @param name the name to use for the result
     *        {@link Configuration} instance
     * @param directory the absolute {@link Path} to use for
     *        {@link FileAppender}s
     * @param level the default {@link Level} for all
     *        {@link Logger}s
     * @param asyncLoggers if asynchronous loggers are used
     * @param loggers the names of the loggers
     * @param appenders the mapping of a logger name to its
     *        appender
     * @param syslogAppenders the mapping of a syslog appender
     *        name to its syslog appender
     * @param appenderFileNamePatterns the mapping of a
     *        {@link Appender} name to its file name pattern
     * @param appenderHeaders the mapping of a {@link Appender}
     *        name to the header of its log file
     * @param appenderFooters the mapping of a {@link Appender}
     *        name to the footer of its log file
     * @param appenderPatterns the mapping of an
     *        {@link Appender} name to its logging
     *        {@link PatternLayout}
     * @param rootAppenders the names of the root appenders for
     *        the {@link LoggerContext} instance
     */
    public Spec(String name, Path directory, Level level, boolean asyncLoggers,
        Map<String, List<String>> loggers, Map<String, String> appenders,
        Map<String, SyslogSpec> syslogAppenders, Map<String, String> appenderFileNamePatterns,
        Map<String, String> appenderHeaders, Map<String, String> appenderFooters,
        Map<String, String> appenderPatterns, Set<String> rootAppenders) {
      this.name = name;
      this.directory = directory;
      this.level = level;
      this.asyncLoggers = asyncLoggers;
      this.loggers = loggers;
      this.appenders = appenders;
      this.syslogAppenders = syslogAppenders;
      this.appenderFileNamePatterns = appenderFileNamePatterns;
      this.appenderHeaders = appenderHeaders;
      this.appenderFooters = appenderFooters;
      this.appenderPatterns = appenderPatterns;
      this.rootAppenders = rootAppenders;
    }

    /**
     * @return the names of the configured {@link Appender}
     */
    Collection<String> getAppenderNames() {
      return this.appenders.keySet();
    }

    Collection<String> getSyslogAppenderNames() {
      return this.syslogAppenders.keySet();
    }

    /**
     * @return the name of the {@link Configuration}
     */
    String getName() {
      return name;
    }

    /**
     * @param appenderName the name of the {@link Appender}
     * @return the configured {@link PatternLayout} pattern
     */
    String getAppenderPattern(String appenderName) {
      return this.appenderPatterns.get(appenderName);
    }

    /**
     * @param appenderName the name of the {@link Appender}
     * @return the header of the log for the appender
     */
    String getAppenderHeader(String appenderName) {
      return this.appenderHeaders.get(appenderName);
    }

    /**
     * @param appenderName the name of the {@link Appender}
     * @return the footer of the log for the appender
     */
    String getAppenderFooter(String appenderName) {
      return this.appenderFooters.get(appenderName);
    }

    /**
     * @param appenderName the name of the {@link Appender}
     * @return the file name for the configured
     *         {@link FileAppender}
     */
    String getAppenderFileName(String appenderName) {
      return appenders.get(appenderName);
    }

    /**
     * @param appenderName the name of the {@link Appender}
     * @return the file name pattern for the configured
     *         {@link FileAppender}
     */
    String getAppenderFileNamePattern(String appenderName) {
      return appenderFileNamePatterns.get(appenderName);
    }

    /**
     * @param appenderName the name of the
     *        {@link SyslogAppender}
     * @return the specification of the {@link SyslogAppender}
     */
    SyslogSpec getSyslogSpec(String appenderName) {
      return this.syslogAppenders.get(appenderName);
    }

    /**
     * @return the names of the configured loggers
     */
    Collection<String> getLoggerNames() {
      return loggers.keySet();
    }

    /**
     * @return the directory for {@link FileAppender}s
     */
    Path getDirectory() {
      return directory;
    }

    /**
     * @param loggerName the name of the {@link Logger}
     * @return the {@link List} of configured {@link Appender}s
     *         for the logger
     */
    List<String> getLoggerAppenders(String loggerName) {
      return loggers.get(loggerName);
    }

    /**
     * @return the names of the {@link Appender} for the root
     *         logger
     */
    Collection<String> getRootAppenders() {
      return rootAppenders;
    }

    /**
     * @return the default {@link Level} for the
     *         {@link LoggerContext}
     */
    Level getLevel() {
      return level;
    }

    /**
     * @return {@code true} if asynchronous loggers are enabled;
     *         otherwise {@code false}
     */
    boolean isAsyncLoggers() {
      return asyncLoggers;
    }
  }

  /**
   * A specification for a {@link SyslogAppender}.
   */
  private static final class SyslogSpec {
    final String name;
    final String host;
    final int port;
    final String protocol;
    final String appName;
    final String facilityName;
    final int enterpriseNumber;
    final String clientHostName;

    /**
     * @param name
     * @param host
     * @param port
     * @param protocol
     * @param appName
     * @param facilityName
     * @param enterpriseNumber
     * @param clientHostName
     */
    SyslogSpec(String name, String host, int port, String protocol, String appName,
        String facilityName, final int enterpriseNumber, String clientHostName) {
      super();
      this.name = name;
      this.host = host;
      this.port = port;
      this.protocol = protocol;
      this.appName = appName;
      this.facilityName = facilityName;
      this.enterpriseNumber = enterpriseNumber;
      this.clientHostName = clientHostName;
    }

  }

  /**
   * An implementation of
   * {@link org.apache.logging.log4j.core.util.Builder} of
   * Apache Log4j for an instance of
   * {@link AbstractConfiguration}. It builds an instance of
   * {@link EmbeddedConfiguration}.
   * 
   * @see Builder#build()
   * @see Builder#configure()
   * @see ConfigurationBuilder#newConfiguration()
   */
  public static class Builder
      implements org.apache.logging.log4j.core.util.Builder<AbstractConfiguration> {

    private static final String NEW_LINE = System.getProperty("line.separator");

    private String name = "builder-default";
    private Path directory;
    private Level level = Level.INFO;
    private Map<String, String> appenderPatterns = new HashMap<>();
    private Map<String, String> appenderHeaders = new HashMap<>();
    private Map<String, String> appenderFooters = new HashMap<>();
    private Map<String, String> appenders = new HashMap<>();
    private Map<String, SyslogSpec> syslogAppenders = new HashMap<>();
    private Map<String, String> appenderFileNamePatterns = new HashMap<>();
    private Map<String, List<String>> loggers = new HashMap<>();
    private Set<String> rootAppenders = new HashSet<>();
    private boolean asyncLoggers = true;


    /**
     * Set the name of the configuration. For an example see
     * {@link DefaultConfiguration#DEFAULT_NAME}.
     * 
     * @param name the name of the {@link Configuration}. By
     *        default is {@code "builder-default"}.
     * @return this builder instance
     */
    public Builder setConfigurationName(String name) {
      this.name = name;
      return this;
    }

    /**
     * @param logDirectory the directory in which the
     *        {@link FileAppender} write logs to
     * @return this builder instance
     */
    public Builder setLogDirectory(Path logDirectory) {
      this.directory = logDirectory;
      return this;
    }

    /**
     * @param level the default {@link Level} for the root
     *        {@link Logger}
     * @return this builder instance
     */
    public Builder setLevel(String level) {
      try {
        this.level = Level.getLevel(level);
        return this;
      } catch (Exception e) {
        throw new IllegalArgumentException("Invalid logging level: " + level, e);
      }
    }

    /**
     * Add a new {@link Appender} that can be a
     * {@link FileAppender} if {@code fileName} is not
     * {@code null}.
     * 
     * @param appenderName the name of the {@link Appender}
     * @param fileName if not {@code null}, the name of the
     *        {@link FileAppender}; e.g. {@code app.log}.
     *        Otherwise the {@link Appender} will be a
     *        {@link ConsoleAppender}
     * @param fileNamePattern if not {@code null}, the file name
     *        pattern for the {@link FileAppender}; e.g.
     *        {@code app.log.%d yyyy-MM-dd} .
     * @return this builder instance
     */
    public Builder addAppender(String appenderName, String fileName, String fileNamePattern) {
      this.appenders.put(appenderName, fileName);
      this.appenderFileNamePatterns.put(appenderName, fileNamePattern);
      return this;
    }

    /**
     * Add a new {@link SyslogAppender} to the configuration.
     * The format is fixed to RFC5424. Syslog appenders are only
     * supported/added to the root logger.
     * 
     * @param appenderName
     * @param host the host name of the receiving Syslog server
     * @param port the port of the receiving Syslog server
     * @param protocol the protocol to send a syslog record;
     *        e.g. <code>UDP</code> or <code>TCP</code>
     * @param appName the name of the application to appear in
     *        the syslog
     * @param facilityName the facility name
     * @param enterpriseNumber the enterprise number
     * @param clientHostName the client host name to appear in
     *        the syslog
     * @return this builder instance
     * 
     * @see SyslogAppender
     */
    public Builder addSyslogAppender(String appenderName, String host, final int port,
        String protocol, String appName, String facilityName, int enterpriseNumber,
        String clientHostName) {
      final SyslogSpec syslogSpec = new SyslogSpec(appenderName, host, port, protocol, appName,
          facilityName, enterpriseNumber, clientHostName);
      this.syslogAppenders.put(appenderName, syslogSpec);
      return this;
    }

    /**
     * Configures a logger with optionally multiple appenders.
     * 
     * @param loggerName the name of the {@link Logger}
     * @param firstAppenderName the name of the first configured
     *        {@link Appender}
     * @param appenderNames the rest of the names of the
     *        {@link Appender}s for the logger
     * @return this builder instance
     */
    public Builder addLogger(String loggerName, String firstAppenderName, String... appenderNames) {
      final List<String> loggerAppenderNames = new ArrayList<>();
      loggers.put(loggerName, loggerAppenderNames);
      loggers.get(loggerName).add(firstAppenderName);
      if (appenderNames != null && appenderNames.length > 0) {
        for (String appender : appenderNames) {
          loggers.get(loggerName).add(appender);
        }
      }
      return this;
    }

    /**
     * Configures the root logger to use an {@link Appender}.
     * 
     * @param appenderName the name of the {@link Appender}
     * @return this builder instance
     */
    public Builder addRootAppender(String appenderName) {
      this.rootAppenders.add(appenderName);
      return this;
    }

    /**
     * Configures the {@link PatternLayout} for an
     * {@link Appender} with no header and footer.
     * 
     * @param appenderName the name of the {@link Appender}
     * @param patternLayout the pattern specified by
     *        {@link PatternLayout} implementations.
     * @return this builder instance
     * 
     * @see #setAppenderPatternLayout(String, String, String,
     *      String)
     */
    public Builder setAppenderPatternLayout(String appenderName, String patternLayout) {
      return setAppenderPatternLayout(appenderName, patternLayout, null, null);
    }

    /**
     * Configures the {@link PatternLayout} for an
     * {@link Appender}.
     * 
     * @param appenderName the name of the {@link Appender}
     * @param patternLayout the pattern specified by
     *        {@link PatternLayout} implementations.
     * @param header the header of the {@link Appender}'s log
     * @param footer the footer of the {@link Appender}'s log
     * @return this builder instance
     */
    public Builder setAppenderPatternLayout(String appenderName, String patternLayout,
        String header, String footer) {
      this.appenderPatterns.put(appenderName, patternLayout);
      if (header != null) {
        this.appenderHeaders.put(appenderName, header + NEW_LINE);
      }
      if (footer != null) {
        this.appenderFooters.put(appenderName, footer + NEW_LINE);
      }
      return this;
    }

    /**
     * Enables JMX logging.
     * 
     * @return this builder instance
     */
    public Builder enableJMX() {
      System.setProperty(LOG4J_DISABLE_JMX, Boolean.FALSE.toString());
      return this;
    }

    /**
     * Currently not implemented.
     * 
     * @return this builder instance
     */
    public Builder enableSyslog() {
      return this;
    }

    /**
     * Disable asynchronous loggers. See
     * {@link org.apache.logging.log4j.core.util.ConfigurationBuilder.LOG4J_ASYNC_LOGGERS}
     * .
     * 
     * @return this builder instance
     */
    public Builder disableAsyncLoggers() {
      this.asyncLoggers = false;
      System.setProperty(LOG4J_ASYNC_LOGGERS,
          org.apache.logging.log4j.core.selector.ClassLoaderContextSelector.class.getName());
      return this;
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.apache.logging.log4j.core.util.Builder#build()
     */
    @Override
    public AbstractConfiguration build() {
      final Spec spec = new Spec(name, directory, level, asyncLoggers, loggers, appenders,
          syslogAppenders, appenderFileNamePatterns, appenderHeaders, appenderFooters,
          appenderPatterns, rootAppenders);
      return new EmbeddedConfiguration(spec);
    }

    /**
     * Builds the configuration with {@link #build()} and then
     * reconfigures the Apache Log4j2 context. The process also
     * installs a shut-down hook to stop the context:
     * 
     * <ul>
     * <li>Builds the configuration into an instance of
     * {@link EmbeddedConfiguration} using {@link #build()}.
     * <li>Sets the default {@link ConfigurationFactory} to
     * {@link EmbeddedConfigurationFactory} using
     * {@link ConfigurationFactory#setConfigurationFactory(ConfigurationFactory)}
     * .
     * <li>If asynchronous loggers are enabled, creates an
     * instance of {@link AsyncLogger}. If not, creates an
     * instance of {@link LoggerContext}.
     * <li>Reconfigures the context using
     * {@link LoggerContext#reconfigure()}.
     * <li>Starts the context using
     * {@link LoggerContext#start()}.
     * <li>Installs a shut down hook to
     * {@link Runtime#addShutdownHook(Thread)} to ensure the
     * logger context is stopped with
     * {@link LoggerContext#stop()}.
     * </ul>
     * 
     * <p>
     * After running this method, the user is able to use Apache
     * Log4j as if it was configured using an instance of
     * {@code log4j.xml}.
     * 
     * @see ConfigurationFactory
     * @see LoggerContext#reconfigure()
     * @see EmbeddedConfigurationFactory
     * @see EmbeddedConfiguration
     */
    public final void configure() {
      final LoggerContext context = asyncLoggers
          ? (AsyncLoggerContext) LogManager.getContext(ConfigurationBuilder.class.getClassLoader(),
              false)
          : (LoggerContext) LogManager.getContext(ConfigurationBuilder.class.getClassLoader(),
              false);
      final AbstractConfiguration configuration = build();
      final EmbeddedConfigurationFactory factory = new EmbeddedConfigurationFactory(configuration);
      ConfigurationFactory.setConfigurationFactory(factory);
      context.reconfigure();
      context.start();
      configuration.start();
      final String rootLoggerName = LogManager.ROOT_LOGGER_NAME;
      final Logger logger = LogManager.getLogger(rootLoggerName);
      logger.info("Initialized logging configuration: {} (async-loggers={})",
          configuration.getName(), logger instanceof AsyncLogger);
      Runtime.getRuntime().addShutdownHook(new Thread(new Runnable() {
        @Override
        public void run() {
          logger.info("Stopping logging configuration: {}", configuration.getName());
          try {
            configuration.stop();
            context.stop();
          } catch (Exception e) {
            System.err.println(e);
          }
        }
      }, rootLoggerName));
    }
  }

  /**
   * An implementation of {@link ConfigurationFactory} for
   * {@link EmbeddedConfiguration}. An instance of this factory
   * works with the provided {@link AbstractConfiguration}
   * instance and always returns the same object when calling
   * {@link ConfigurationFactory#getConfiguration(ConfigurationSource)}
   * or
   * {@link ConfigurationFactory#getConfiguration(String, URI, ClassLoader)}
   * .
   */
  public static final class EmbeddedConfigurationFactory extends ConfigurationFactory {

    private final AbstractConfiguration configuration;

    /**
     * Ctor.
     * 
     * @param configuration
     */
    public EmbeddedConfigurationFactory(AbstractConfiguration configuration) {
      this.configuration = configuration;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.apache.logging.log4j.core.config.ConfigurationFactory
     * #getSupportedTypes()
     */
    @Override
    protected String[] getSupportedTypes() {
      return new String[] {"*"};
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.apache.logging.log4j.core.config.ConfigurationFactory
     * #getConfiguration(org.apache.logging.log4j.core.config.
     * ConfigurationSource)
     */
    @Override
    public Configuration getConfiguration(ConfigurationSource source) {
      return configuration;
    }

    @Override
    public Configuration getConfiguration(String name, URI configLocation) {
      return configuration;
    }

    /*
     * (non-Javadoc)
     * 
     * @see
     * org.apache.logging.log4j.core.config.ConfigurationFactory
     * #getConfiguration(java.lang.String, java.net.URI,
     * java.lang.ClassLoader)
     */
    @Override
    public Configuration getConfiguration(String name, URI configLocation, ClassLoader loader) {
      return configuration;
    }

  }

  /**
   * An extension of {@link AbstractConfiguration} for an
   * embedded setup. See
   * {@link EmbeddedConfiguration#EmbeddedConfiguration(Spec)}.
   * 
   * @see Builder
   * @see Builder#configure()
   * @see Spec
   */
  private static final class EmbeddedConfiguration extends AbstractConfiguration {
    private static final long serialVersionUID = 1L;

    /**
     * Ctor. The configuration is built using instance of
     * {@link Spec}:
     * 
     * <ul>
     * <li>The name is set to {@link Spec#getName()}.
     * <li>Appenders from the parent object are cleared.
     * <li>{@link Appender}s are created
     * {@link #buildAppenders(Spec)}.
     * <li>{@link LoggerConfig}s are created using
     * {@link #buildLoggerConfigs(Spec)}.
     * <li>Every {@link LoggerConfig} is configured with its
     * {@link Appender} using
     * {@link #configureLoggerAppenders(Spec, Map, Map)}.
     * <li>Root loggers are configured.
     * </ul>
     * 
     * @param spec
     */
    EmbeddedConfiguration(Spec spec) {
      super(ConfigurationSource.NULL_SOURCE);
      setName(spec.getName());

      // Clean up first
      getAppenders().clear();
      getRootLogger().getAppenders().clear();

      // Build Appenders
      final Map<String, Appender> appenders = buildAppenders(spec);
      final Map<String, SyslogAppender> syslogAppenders = buildSyslogAppenders(spec);

      // Build Logger Configs
      final Map<String, LoggerConfig> loggers = buildLoggerConfigs(spec);

      // Configure loggers with appenders
      configureLoggerAppenders(spec, appenders, loggers);

      // Configure root logger appenders
      configureRootLogger(spec, appenders, syslogAppenders);
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.apache.logging.log4j.core.config.
     * AbstractConfiguration #doConfigure()
     */
    @Override
    protected void doConfigure() {
      // Nothing for this configuration and remove `super`
      // logic.
    }

    /**
     * Configures the root logger with the specified appenders
     * in the configuration.
     * 
     * @param spec
     * @param appenders the built {@link Appender}s in the
     *        configuration
     * @param syslogAppenders the built {@link SyslogAppender}s
     *        in the configuration
     */
    protected void configureRootLogger(Spec spec, final Map<String, Appender> appenders,
        Map<String, SyslogAppender> syslogAppenders) {
      for (String appenderName : spec.getRootAppenders()) {
        Appender appender = appenders.get(appenderName);
        getRootLogger().addAppender(appender, spec.getLevel(), null);
      }
      for (SyslogAppender appender : syslogAppenders.values()) {
        getRootLogger().addAppender(appender, spec.getLevel(), null);
      }
      getRootLogger().setLevel(spec.getLevel());
    }

    /**
     * Configures the {@link LoggerConfig} with their associated
     * {@link Appender}s.
     * 
     * @param spec
     * @param appenders the created {@link Appender}s from the
     *        configuration
     * @param loggers the created {@link LoggerConfig}s from the
     *        configuration
     */
    protected void configureLoggerAppenders(Spec spec, final Map<String, Appender> appenders,
        final Map<String, LoggerConfig> loggers) {
      for (String loggerName : loggers.keySet()) {
        List<String> loggerAppenderNames = spec.getLoggerAppenders(loggerName);
        for (String appenderName : loggerAppenderNames) {
          Appender appender = appenders.get(appenderName);
          LoggerConfig loggerConfig = loggers.get(loggerName);
          loggerConfig.addAppender(appender, spec.getLevel(), null);
          addLogger(loggerName, loggerConfig);
        }
      }
    }

    /**
     * Builds all the {@link LoggerConfig}s according to the
     * configuration:
     * 
     * <ul>
     * <li>Every {@link LoggerConfig} uses the same
     * {@link Level} specified by {@link Spec#getLevel()}.
     * <li>Logger additivity is by default disabled.
     * </ul>
     * 
     * @param spec the instance of {@link Spec}
     * @return a {@link Map} of the names of the loggers to
     *         {@link LoggerConfig}
     */
    protected Map<String, LoggerConfig> buildLoggerConfigs(Spec spec) {
      final Map<String, LoggerConfig> loggers = new HashMap<>();
      final Collection<String> loggerNames = spec.getLoggerNames();
      for (String loggerName : loggerNames) {
        Level level = spec.getLevel();
        boolean additivity = false;
        LoggerConfig logger =
            createLoggerConfig(this, spec.isAsyncLoggers(), loggerName, level, additivity);
        loggers.put(loggerName, logger);
      }
      return loggers;
    }

    /**
     * Builds all the {@link Appender}s in the configuration:
     * <ul>
     * <li>If an appender name is {@code null}, it is considered
     * as a {@link ConsoleAppender}.
     * <li>By default, a {@link FileAppender} is configured with
     * <i>daily</i> rollover strategy and a <i>30</i> maximum
     * files to keep strategy. See
     * {@link ConfigurationBuilder#createFileAppender(Configuration, String, String, String, PatternLayout)}
     * .
     * </ul>
     * 
     * @param spec the instance of {@link Spec}
     * @return a {@link Map} of the name of appenders to their
     *         {@link Appender} instance
     */
    protected Map<String, Appender> buildAppenders(Spec spec) {
      final Map<String, Appender> appenders = new HashMap<>();
      final Collection<String> appenderNames = spec.getAppenderNames();
      for (String appenderName : appenderNames) {
        String pattern = spec.getAppenderPattern(appenderName);
        String header = spec.getAppenderHeader(appenderName);
        String footer = spec.getAppenderFooter(appenderName);
        PatternLayout layout = createLayout(this, header, footer, pattern);
        String appenderFileName = spec.getAppenderFileName(appenderName);
        String appenderFileNamePattern = spec.getAppenderFileNamePattern(appenderName);
        Appender appender = null;
        if (appenderFileName == null) {
          // This is a "console" appender.
          appender = createConsoleAppender(layout, appenderName);
        } else {
          String fileName =
              spec.getDirectory().resolve(appenderFileName).toAbsolutePath().toString();
          String fileNamePattern =
              spec.getDirectory().resolve(appenderFileNamePattern).toAbsolutePath().toString()
                  + ".gz";
          // XXX Candidate to be added to the builder API.
          String rolloverInterval = "1";
          String retentionInterval = "30";
          appender = createFileAppender(this, fileName, fileNamePattern, appenderName, layout,
              rolloverInterval, retentionInterval);
        }
        appenders.put(appenderName, appender);
        addAppender(appender);
      }
      return appenders;
    }

    /**
     * @param spec
     * @return
     */
    protected Map<String, SyslogAppender> buildSyslogAppenders(Spec spec) {
      Map<String, SyslogAppender> syslogAppenders = new HashMap<>();
      Collection<String> syslogAppenderNames = spec.getSyslogAppenderNames();
      for (String appenderName : syslogAppenderNames) {
        SyslogSpec sspec = spec.getSyslogSpec(appenderName);
        SyslogAppender syslogAppender =
            createSyslogAppender(sspec.name, sspec.host, sspec.port, sspec.protocol, sspec.appName,
                this, sspec.facilityName, sspec.enterpriseNumber, sspec.clientHostName);
        syslogAppenders.put(appenderName, syslogAppender);
        addAppender(syslogAppender);
      }
      return syslogAppenders;
    }



  }

  /**
   * Creates a new instance of {@link Builder} that helps
   * building an embedded configuration instance for Apache
   * Log4j2 and then configures it.
   * 
   * @return the {@link Builder} instance
   */
  public static Builder newConfiguration() {
    return new Builder();
  }

  /**
   * Creates a {@link PatternLayout}:
   * <ul>
   * <li>Exceptions are written
   * <li>No regex replacement is used.
   * </ul>
   * 
   * @see PatternLayout#newBuilder()
   * 
   * @param configuration the owner configuration
   * @param header the header of the pattern layout
   * @param footer the footer of the pattern layout
   * @param pattern the pattern of the log messages
   * @return the built {@link PatternLayout}
   */
  protected static PatternLayout createLayout(final Configuration configuration,
      final String header, final String footer, final String pattern) {
    return PatternLayout.newBuilder().withAlwaysWriteExceptions(true)
        .withConfiguration(configuration).withHeader(header).withFooter(footer).withPattern(pattern)
        .withRegexReplacement(null).build();
  }

  /**
   * Creates a {@link FileAppender}.
   * 
   * @param configuration the owner configuration
   * @param fileName the name of log file
   * @param fileNamePattern the pattern of the name of the log
   *        file
   * @param appenderName the name of the appender
   * @param layout the {@link PatternLayout} to use for the
   *        appender
   * @param rolloverInterval how often the log files should be
   *        rolled over (in DAYS)
   * @param maximumFilesToKeep the maximum number of file to
   *        keep after every roll-over
   * @return an instance of {@link Appender}
   */
  protected static Appender createFileAppender(final Configuration configuration,
      final String fileName, final String fileNamePattern, final String appenderName,
      final PatternLayout layout, String rolloverInterval, String maximumFilesToKeep) {
    final String append = Boolean.TRUE.toString();
    final String bufferedIO = Boolean.TRUE.toString();
    final String bufferSizeStr = null;
    final String immediateFlush = Boolean.TRUE.toString();
    final Filter filter = null;
    final String ignore = null;
    final String advertise = null;
    final String advertiseURI = null;

    // Trigger Policy
    final String modulate = Boolean.TRUE.toString();
    final TriggeringPolicy policy =
        TimeBasedTriggeringPolicy.createPolicy(rolloverInterval, modulate);

    // Rollover strategy
    final String minFilesToKeep = "1";
    final String fileIndex = null;
    final String compressionLevelStr = Integer.toString(Deflater.DEFAULT_COMPRESSION);
    final RolloverStrategy rolloverStrategy = DefaultRolloverStrategy.createStrategy(
        maximumFilesToKeep, minFilesToKeep, fileIndex, compressionLevelStr, configuration);

    return RollingFileAppender.createAppender(fileName, fileNamePattern, append, appenderName,
        bufferedIO, bufferSizeStr, immediateFlush, policy, rolloverStrategy, layout, filter, ignore,
        advertise, advertiseURI, configuration);
  }

  /**
   * Creates an instance of {@link ConsoleAppender}.
   * 
   * @param layout the {@link PatternLayout} to use
   * @param name the name of the appender
   * @return an instance of {@link ConsoleAppender}
   */
  protected static ConsoleAppender createConsoleAppender(PatternLayout layout, String name) {
    Filter filter = null;
    String targetStr = Target.SYSTEM_OUT.toString();
    String follow = Boolean.FALSE.toString();
    String ignore = Boolean.FALSE.toString();
    return ConsoleAppender.createAppender(layout, filter, targetStr, name, follow, ignore);
  }

  /**
   * Creates an instance of {@link SyslogAppender}. Parameters
   * match that of appearing in the same method in
   * {@link SyslogAppender}.
   * 
   * @param name
   * @param host
   * @param port
   * @param protocolStr
   * @param appName
   * @param config
   * @param facilityName
   * @param enterpriseNumber
   * @param clientHostName
   * @return an instance of {@link SyslogAppender}
   */
  protected static SyslogAppender createSyslogAppender(String name, String host, int port,
      String protocolStr, String appName, Configuration config, String facilityName,
      int enterpriseNumber, String clientHostName) {
    final SslConfiguration sslConfig = null;
    int connectTimeoutMillis = 0;
    int reconnectionDelayMillis = 0;
    boolean immediateFail = true;
    boolean immediateFlush = true;
    boolean ignoreExceptions = true;
    Facility facility = Facility.toFacility(facilityName, Facility.USER);
    String id = null;
    boolean includeMdc = false;
    String mdcId = "mdc-ignored-id";
    String mdcPrefix = null;
    String eventPrefix = "|/" + clientHostName + "/" + appName + ".log|";
    boolean newLine = true;
    String escapeNL = null;
    String msgId = appName;
    String mdcExcludes = null;
    String mdcIncludes = null;
    String mdcRequired = null;
    String format = "RFC5424";
    Filter filter = null;
    Charset charsetName = Charset.defaultCharset();
    String exceptionPattern = null;
    LoggerFields[] loggerFields = new LoggerFields[0];
    boolean advertise = false;
    SyslogAppender sla = SyslogAppender.createAppender(host, port, protocolStr, sslConfig,
        connectTimeoutMillis, reconnectionDelayMillis, immediateFail, name, immediateFlush,
        ignoreExceptions, facility, id, enterpriseNumber, includeMdc, mdcId, mdcPrefix, eventPrefix,
        newLine, escapeNL, appName, msgId, mdcExcludes, mdcIncludes, mdcRequired, format, filter,
        config, charsetName, exceptionPattern, loggerFields, advertise);
    return sla;
  }

  /**
   * Creates an instance of {@link LoggerConfig} which may be
   * {@link AsyncLoggerConfig} if asynchronous loggers are used.
   * 
   * @param configuration the owner configuration
   * @param asyncLoggers
   * @param loggerName the name of the logger
   * @param level the {@link Level} of the logger
   * @param additivity if additivity is enabled for the logger
   * @return an instance of {@link LoggerConfig} or
   *         {@link AsyncLoggerConfig}
   */
  protected static LoggerConfig createLoggerConfig(final Configuration configuration,
      boolean asyncLoggers, final String loggerName, final Level level, final boolean additivity) {
    final Filter filter = null;
    if (asyncLoggers) {
      // XXX Obscure static factory methods.
      return new AsyncLoggerConfig(loggerName, Collections.<AppenderRef>emptyList(), filter, level,
          additivity, new Property[0], configuration, false) {
        private static final long serialVersionUID = 1L;
      };
    } else {
      return LoggerConfig.createLogger(String.valueOf(additivity), level, loggerName, null,
          new AppenderRef[0], new Property[0], configuration, filter);
    }
  }
}
