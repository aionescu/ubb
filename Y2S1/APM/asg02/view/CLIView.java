package asg02.view;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import asg02.controller.Controller;
import asg02.exn.InvalidArgumentException;

public final class CLIView implements View {
  private static final class Pair<A, B> {
    public final A fst;
    public final B snd;

    public Pair(A a, B b) {
      fst = a;
      snd = b;
    }
  }

  private final Controller _controller;
  private final Map<String, Method> _cmds;

  public CLIView(Controller controller) {
    if (controller == null)
      throw new InvalidArgumentException();

    _controller = controller;
    _cmds = new HashMap<String, Method>();

    _loadCmds();
  }

  private void _loadCmds() {
    Arrays.stream(CLIView.class.getDeclaredMethods())
    .filter(m ->
      m.getName().startsWith("_cmd"))
    .forEach(m -> {
      var name =
        m.getName().substring(4)
        .replaceAll("(.)(\\p{Upper})", "$1-$2").toLowerCase();

      _cmds.put(name, m);
    });
  }

  private static boolean _parseBool(String s) {
    return switch (s.toLowerCase()) {
      case "true" -> true;
      case "false" -> false;
      default -> throw new InvalidArgumentException();
    };
  }

  private static void _showExn(Throwable e) {
    var message =
      switch (e.getClass().getSimpleName()) {
        case "ArrayIndexOutOfBoundsException" -> "Not enough arguments were specified";
        case "DuplicateAttendeeException" -> "An attendee with that name already exists";
        case "InvalidAttendeeException", "InvalidArgumentException" -> "Some arguments were not in the correct format";
        case "InexistentAttendeeException" -> "No attendees with that name exist";
        case "RepositoryFullException" -> "The repository is at full capacity";
        default -> e.toString();
      };

    System.out.println("Error: " + message + ".");
  }

  @SuppressWarnings("unused")
  private void _cmdHelp(String[] args) {
    System.out.println("Available commands:");
    _cmds.keySet().stream().forEach(System.out::println);
  }

  @SuppressWarnings("unused")
  private void _cmdExit(String[] args) {
    System.exit(0);
  }

  @SuppressWarnings("unused")
  private void _cmdEcho(String[] args) {
    Arrays.stream(args).forEach(System.out::println);
  }

  @SuppressWarnings("unused")
  private void _cmdShow(String[] args) {
    _controller.attendees().forEach(System.out::println);
  }

  @SuppressWarnings("unused")
  private void _cmdShowFiltered(String[] args) {
    _controller
      .attendeesWithPresentedWorks(_parseBool(args[0]))
      .forEach(System.out::println);
  }

  @SuppressWarnings("unused")
  private void _cmdAddStudent(String[] args) {
    _controller.addStudent(args[0], args[1]);
  }

  @SuppressWarnings("unused")
  private void _cmdAddProfessor(String[] args) {
    _controller.addProfessor(args[0], args[1]);
  }

  @SuppressWarnings("unused")
  private void _cmdAddIndustrySpecialist(String[] args) {
    _controller.addIndustrySpecialist(args[0], args[1], _parseBool(args[2]));
  }

  @SuppressWarnings("unused")
  private void _cmdRemoveAttendee(String[] args) {
    _controller.remove(args[0]);
  }

  private static Pair<String, String[]> _parseCommand(String line) {
    line = line.trim();

    var spaceIndex = line.indexOf(' ');

    String cmd, args[];

    if (spaceIndex == -1) {
      cmd = line;
      args = new String[] { };
    } else {
      cmd = line.substring(0, spaceIndex);
      args =
        Arrays.stream(line.substring(spaceIndex).split(","))
        .map(String::trim)
        .toArray(String[]::new);
    }

    return new Pair<>(cmd, args);
  }

  private void _runCommand(String line) {
    var pair = _parseCommand(line);
    var cmd = pair.fst;
    var args = pair.snd;

    if (cmd.isEmpty())
      return;

    var mtd = _cmds.get(cmd);

    if (mtd == null)
      System.out.println("Command not recognized.");
    else
      try {
        mtd.invoke(this, new Object[] { args });
      } catch (InvocationTargetException e) {
        _showExn(e.getTargetException());
      } catch (Exception e) {
        _showExn(e);
      }
  }

  @Override
  public void run() {
    var console = System.console();

    while (true) {
      System.out.print("\nconf> ");
      var line = console.readLine();
      _runCommand(line);
    }
  }
}
