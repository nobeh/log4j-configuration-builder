package org.apache.logging.log4j.core.util;

import static org.junit.Assert.assertTrue;

import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.common.io.CharStreams;

public class ConfigurationBuilderTest {

  private Path directory;

  @Before
  public void before() throws Exception {
    directory = Files.createTempDirectory("log-" + System.currentTimeMillis() + "-");
  }

  @After
  public void after() throws Exception {
    for (Path p : Files.newDirectoryStream(directory)) {
      Files.deleteIfExists(directory.resolve(p));
    }
    Files.deleteIfExists(directory);
  }

  @Test
  public void testBuildConfigure() throws Exception {
    String header = " *** START *** ";
    String footer = " *** END *** ";
    String fileName = "test.log";
    String msg = "It works!";

    ConfigurationBuilder.newConfiguration().setConfigurationName("test")
        .setAppenderPatternLayout("default", "%t %p %d %m %n", header, footer)
        .setLogDirectory(directory).addAppender("default", fileName, "test.log-%d{yyyy.MM.dd}")
        .addLogger("default", "default").addRootAppender("default").configure();

    Logger logger = LoggerFactory.getLogger(ConfigurationBuilderTest.class);
    logger.info(msg);

    Thread.sleep(1000);
    String logs =
        CharStreams.toString(Files.newBufferedReader(directory.resolve(fileName),
            Charset.defaultCharset()));

    assertTrue(logs.contains(header));
    assertTrue(logs.contains(msg));
  }

  @Test
  public void testBuildConfigureMultipleAppenders() throws Exception {
    String header = " *** START *** ";
    String footer = " *** END *** ";
    String fileName = "test.log";
    String fileName2 = "zest.log";
    String msg = "It works!";
    String msg2 = "It works! 2";

    String layout = "%t %p %d %m (%logger{1}:%L)%n";
    String appender1Name = "a1";
    String appender2Name = "a2";
    ConfigurationBuilder.newConfiguration().setConfigurationName("test").setLogDirectory(directory)
        .addAppender(appender1Name, fileName, "test.log-%d{yyyy.MM.dd}")
        .setAppenderPatternLayout(appender1Name, layout, header, footer)
        .addAppender(appender2Name, fileName2, "zest.log-%d{yyyy.MM.dd}")
        .setAppenderPatternLayout(appender2Name, layout).addLogger("zest", appender2Name)
        .addRootAppender(appender1Name).configure();

    Logger logger1 = LoggerFactory.getLogger(getClass());
    Logger logger2 = LoggerFactory.getLogger("zest");
    logger1.info(msg);
    logger2.info(msg2);

    Thread.sleep(1000);
    String logs =
        CharStreams.toString(Files.newBufferedReader(directory.resolve(fileName),
            Charset.defaultCharset()));
    assertTrue(logs.contains(header));
    assertTrue(logs.contains(msg));

    String logs2 =
        CharStreams.toString(Files.newBufferedReader(directory.resolve(fileName2),
            Charset.defaultCharset()));
    assertTrue(logs2.contains(msg2));
  }
}
