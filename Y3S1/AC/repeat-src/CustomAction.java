package core;

import core.userDefinedTask.UserDefinedAction;
import core.userDefinedTask.SharedVariables;
import core.userDefinedTask.Tools;
import core.controller.Core;
import core.controller.MouseCore;
import core.controller.KeyboardCore;
import core.keyChain.ActivationPhrase;
import core.keyChain.KeyChain;
import core.keyChain.KeySequence;
import core.keyChain.KeyStroke;
import core.keyChain.ButtonStroke;
import core.keyChain.ButtonStroke.Source;
import core.keyChain.MouseGesture;
import core.keyChain.TaskActivation;
import core.userDefinedTask.TaskInvoker;
import core.userDefinedTask.internals.SharedVariablesSubscription;
import java.util.List;
import java.util.HashMap;
import java.util.ArrayList;
import java.awt.Point;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.time.Duration;
import java.time.Instant;

import static java.awt.event.KeyEvent.*;
import static java.awt.event.InputEvent.BUTTON1_MASK;
import static java.awt.event.InputEvent.BUTTON3_MASK;

public class CustomAction extends UserDefinedAction {
  private File recordingFile = new File("/home/oldpug/Desktop/Study/Keys.csv");
  private File recordingFileStop = new File("/home/oldpug/Desktop/Study/Stop");
  private FileWriter fileWriter;

  private HashMap<ButtonStroke, Instant> pressedKeys = new HashMap<>();

  private void record(Object... cols) {
    try {
      for (var i = 0; i < cols.length; ++i) {
        fileWriter.append(cols[i].toString());
        fileWriter.append(i == cols.length - 1 ? "\n" : ",");
      }

      fileWriter.flush();
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  private void initFileWriter() {
    try {
      recordingFile.getParentFile().mkdirs();
      recordingFile.createNewFile();

      fileWriter = new FileWriter(recordingFile, false);
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  public synchronized void action(final Core c) throws InterruptedException {
    var now = Instant.now();

    if (fileWriter == null)
      initFileWriter();

    if (recordingFileStop.exists()) {
      recordingFile.delete();
      recordingFileStop.delete();
      initFileWriter();
    }

    if (invokingKeyChain == null || invokingKeyChain.getButtonStrokes().size() == 0)
      return;

    var currentStroke = invokingKeyChain.getButtonStrokes().get(0);

    if (currentStroke.isPressed()) {
      if (!pressedKeys.containsKey(currentStroke))
        pressedKeys.put(currentStroke, now);
    } else {
      if (pressedKeys.containsKey(currentStroke)) {
        var duration = Duration.between(pressedKeys.get(currentStroke), now);
        record(currentStroke.getKey(), duration.toMillis(), currentStroke.getSource() == Source.MOUSE);
        pressedKeys.remove(currentStroke);
      }
    }
  }

  @Override
  public String getNamespace() {
    return SharedVariables.GLOBAL_NAMESPACE;
  }
}
