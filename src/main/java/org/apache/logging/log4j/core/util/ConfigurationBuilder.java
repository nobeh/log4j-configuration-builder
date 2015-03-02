package org.apache.logging.log4j.core.util;

import java.net.URI;
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
import org.apache.logging.log4j.core.appender.RollingFileAppender;
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
import org.apache.logging.log4j.core.layout.PatternLayout;

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
   * @see <a
   *      href="http://logging.apache.org/log4j/2.0/manual/async.html">Async
   *      Loggers</a>
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
    private final Level level;
    private final Path directory;
    private final Map<String, List<String>> loggers;
    private final Map<String, String> appenders;
    private final Map<String, String> appenderFileNamePatterns;
    private final Map<String, String> appenderHeaders;
    private final Map<String, String> appenderFooters;
    private final Map<String, String> appenderPatterns;
    private final Set<String> rootAppenders;

    public Spec(String name, Path directory, Level level, Map<String, List<String>> loggers,
        Map<String, String> appenders, Map<String, String> appenderFileNamePatterns,
        Map<String, String> appenderHeaders, Map<String, String> appenderFooters,
        Map<String, String> appenderPatterns, Set<String> rootAppenders) {
      this.name = name;
      this.directory = directory;
      this.level = level;
      this.loggers = loggers;
      this.appenders = appenders;
      this.appenderFileNamePatterns = appenderFileNamePatterns;
      this.appenderHeaders = appenderHeaders;
      this.appenderFooters = appenderFooters;
      this.appenderPatterns = appenderPatterns;
      this.rootAppenders = rootAppenders;
    }

    Collection<String> getAppenderNames() {
      return this.appenders.keySet();
    }

    String getName() {
      return name;
    }

    String getAppenderPattern(String appenderName) {
      return this.appenderPatterns.get(appenderName);
    }

    String getAppenderHeader(String appenderName) {
      return this.appenderHeaders.get(appenderName);
    }

    String getAppenderFooter(String appenderName) {
      return this.appenderFooters.get(appenderName);
    }

    String getAppenderFileName(String appenderName) {
      return appenders.get(appenderName);
    }

    String getAppenderFileNamePattern(String appenderName) {
      return appenderFileNamePatterns.get(appenderName);
    }

    Collection<String> getLoggerNames() {
      return loggers.keySet();
    }

    Path getDirectory() {
      return directory;
    }

    List<String> getLoggerAppenders(String loggerName) {
      return loggers.get(loggerName);
    }

    Collection<String> getRootAppenders() {
      return rootAppenders;
    }

    Level getLevel() {
      return level;
    }

  }

  /**
   * An implementation of
   * {@link org.apache.logging.log4j.core.util.Builder} of
   * Apache Log4j for an instance of
   * {@link AbstractConfiguration}.
   * 
   * @see Builder#build()
   * @see Builder#configure()
   * @see ConfigurationBuilder#newConfiguration()
   */
  public static class Builder implements
      org.apache.logging.log4j.core.util.Builder<AbstractConfiguration> {

    private static final String NEW_LINE = System.getProperty("line.separator");

    private String name = "builder-default";
    private Path directory;
    private Level level = Level.INFO;
    private Map<String, String> appenderPatterns = new HashMap<>();
    private Map<String, String> appenderHeaders = new HashMap<>();
    private Map<String, String> appenderFooters = new HashMap<>();
    private Map<String, String> appenders = new HashMap<>();
    private Map<String, String> appenderFileNamePatterns = new HashMap<>();
    private Map<String, List<String>> loggers = new HashMap<>();
    private Set<String> rootAppenders = new HashSet<>();
    private boolean asyncLoggers = true;

    /**
     * Set the name of the configuration. For an example see
     * {@link DefaultConfiguration#DEFAULT_NAME}.
     * 
     * @param name
     * @return this builder instance
     */
    public Builder setConfigurationName(String name) {
      this.name = name;
      return this;
    }

    public Builder setLogDirectory(Path logDirectory) {
      this.directory = logDirectory;
      return this;
    }

    public Builder setLevel(String level) {
      try {
        this.level = Level.getLevel(level);
        return this;
      } catch (Exception e) {
        throw new IllegalArgumentException("Invalid logging level: " + level, e);
      }
    }

    public Builder addAppender(String appenderName, String fileName, String fileNamePattern) {
      this.appenders.put(appenderName, fileName);
      this.appenderFileNamePatterns.put(appenderName, fileNamePattern);
      return this;
    }

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

    public Builder addRootAppender(String appenderName) {
      this.rootAppenders.add(appenderName);
      return this;
    }

    public Builder setAppenderPatternLayout(String appenderName, String patternLayout) {
      return setAppenderPatternLayout(appenderName, patternLayout, null, null);
    }

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
     * @return
     */
    public Builder enableJMX() {
      System.setProperty(LOG4J_DISABLE_JMX, Boolean.FALSE.toString());
      return this;
    }

    /**
     * @return
     */
    public Builder enableSyslog() {
      return this;
    }

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
      final Spec spec =
          new Spec(name, directory, level, loggers, appenders, appenderFileNamePatterns,
              appenderHeaders, appenderFooters, appenderPatterns, rootAppenders);
      return new EmbeddedConfiguration(spec);
    }

    /**
     * Builds the configuration with {@link #build()} and then
     * reconfigures the Apache Log4j2 context. The process also
     * installs a shut-down hook to stop the context.
     * 
     * @see ConfigurationFactory
     * @see LoggerContext#reconfigure()
     * @see EmbeddedConfigurationFactory
     * @see EmbeddedConfiguration
     */
    public void configure() {
      LoggerContext context;
      if (asyncLoggers) {
        context =
            (AsyncLoggerContext) LogManager.getContext(ConfigurationBuilder.class.getClassLoader(),
                false);
      } else {
        context =
            (LoggerContext) LogManager.getContext(ConfigurationBuilder.class.getClassLoader(),
                false);
      }
      final AbstractConfiguration configuration = build();
      final EmbeddedConfigurationFactory factory = new EmbeddedConfigurationFactory(configuration);
      ConfigurationFactory.setConfigurationFactory(factory);
      context.reconfigure();
      context.start();
      final String rootLoggerName = LogManager.ROOT_LOGGER_NAME;
      final Logger logger = LogManager.getLogger(rootLoggerName);
      logger.info("Initialized logging configuration: {} asyncLoggers={}", configuration.getName(),
          logger instanceof AsyncLogger);
      Runtime.getRuntime().addShutdownHook(new Thread(new Runnable() {
        @Override
        public void run() {
          logger.info("Stopping logging configuration: {}", configuration.getName());
          context.stop();
        }
      }, rootLoggerName));
    }
  }

  /**
   * An implementation of {@link ConfigurationFactory} for
   * {@link EmbeddedConfiguration}.
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

    @Override
    protected String[] getSupportedTypes() {
      return new String[] {"*"};
    }

    @Override
    public Configuration getConfiguration(ConfigurationSource source) {
      return configuration;
    }

    @Override
    public Configuration getConfiguration(String name, URI configLocation) {
      return configuration;
    }

    @Override
    public Configuration getConfiguration(String name, URI configLocation, ClassLoader loader) {
      return configuration;
    }

  }

  /**
   * An extension of {@link AbstractConfiguration} for an
   * embedded setup.
   * 
   * @see Builder
   * @see Builder#configure()
   * @see Spec
   */
  private static final class EmbeddedConfiguration extends AbstractConfiguration {
    private static final long serialVersionUID = 1L;

    /**
     * Ctor.
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

      // Build Logger Configs
      final Map<String, LoggerConfig> loggers = buildLoggerConfigs(spec);

      // Configure loggers with appenders
      configureLoggerAppenders(spec, appenders, loggers);

      // Configure root logger appenders
      configureRootLogger(spec, appenders);
    }

    @Override
    protected void doConfigure() {
      // Nothing for this configuration and remove `super`
      // logic.
    }

    /**
     * @param spec
     * @param appenders
     */
    protected void configureRootLogger(Spec spec, final Map<String, Appender> appenders) {
      for (String appenderName : spec.getRootAppenders()) {
        Appender appender = appenders.get(appenderName);
        getRootLogger().addAppender(appender, spec.getLevel(), null);
      }
      getRootLogger().setLevel(spec.getLevel());
    }

    /**
     * @param spec
     * @param appenders
     * @param loggers
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
     * @param spec
     * @return
     */
    protected Map<String, LoggerConfig> buildLoggerConfigs(Spec spec) {
      final Map<String, LoggerConfig> loggers = new HashMap<>();
      final Collection<String> loggerNames = spec.getLoggerNames();
      for (String loggerName : loggerNames) {
        Level level = spec.getLevel();
        boolean additivity = false;
        LoggerConfig logger = createLoggerConfig(this, loggerName, level, additivity);
        loggers.put(loggerName, logger);
      }
      return loggers;
    }

    /**
     * @param spec
     * @return
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
          appender = createAppender(this, fileName, fileNamePattern, appenderName, layout);
        }
        appenders.put(appenderName, appender);
        addAppender(appender);
      }
      return appenders;
    }

  }

  /**
   * Creates a new instance of {@link Builder} that helps
   * building an embedded configuration instance for Apache
   * Log4j2 and then configures it.
   * 
   * @return
   */
  public static Builder newConfiguration() {
    return new Builder();
  }

  /**
   * @param configuration
   * @param header
   * @param footer
   * @param pattern
   * @return
   */
  protected static PatternLayout createLayout(final Configuration configuration,
      final String header, final String footer, final String pattern) {
    return PatternLayout.newBuilder().withAlwaysWriteExceptions(true)
        .withConfiguration(configuration).withHeader(header).withFooter(footer)
        .withPattern(pattern).withRegexReplacement(null).build();
  }

  /**
   * @param configuration
   * @param fileName
   * @param fileNamePattern
   * @param appenderName
   * @param layout
   * @return
   */
  protected static Appender createAppender(final Configuration configuration,
      final String fileName, final String fileNamePattern, final String appenderName,
      final PatternLayout layout) {
    final String append = Boolean.TRUE.toString();
    final String bufferedIO = Boolean.TRUE.toString();
    final String bufferSizeStr = null;
    final String immediateFlush = Boolean.TRUE.toString();
    final Filter filter = null;
    final String ignore = null;
    final String advertise = null;
    final String advertiseURI = null;

    // Trigger Policy
    final String interval = "1"; // 1 day
    final String modulate = Boolean.TRUE.toString();
    final TriggeringPolicy policy = TimeBasedTriggeringPolicy.createPolicy(interval, modulate);

    // Rollover strategy
    final String maxFilesToKeep = "30";
    final String minFilesToKeep = "1";
    final String fileIndex = null;
    final String compressionLevelStr = Integer.toString(Deflater.DEFAULT_COMPRESSION);
    final RolloverStrategy rolloverStrategy =
        DefaultRolloverStrategy.createStrategy(maxFilesToKeep, minFilesToKeep, fileIndex,
            compressionLevelStr, configuration);

    return RollingFileAppender.createAppender(fileName, fileNamePattern, append, appenderName,
        bufferedIO, bufferSizeStr, immediateFlush, policy, rolloverStrategy, layout, filter,
        ignore, advertise, advertiseURI, configuration);
  }

  /**
   * @param layout
   * @param name
   * @return
   */
  protected static ConsoleAppender createConsoleAppender(PatternLayout layout, String name) {
    Filter filter = null;
    String targetStr = Target.SYSTEM_OUT.toString();
    String follow = Boolean.FALSE.toString();
    String ignore = Boolean.FALSE.toString();
    return ConsoleAppender.createAppender(layout, filter, targetStr, name, follow, ignore);
  }

  /**
   * @param configuration
   * @param loggerName
   * @param level
   * @param additivity
   * @return
   */
  protected static LoggerConfig createLoggerConfig(final Configuration configuration,
      final String loggerName, final Level level, final boolean additivity) {
    final Filter filter = null;
    // Obscure static factory methods.
    return new AsyncLoggerConfig(loggerName, Collections.<AppenderRef>emptyList(), filter, level,
        additivity, new Property[0], configuration, false) {
      private static final long serialVersionUID = 1L;
    };
  }
}
