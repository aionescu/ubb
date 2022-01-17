#include <cstddef>
#include <fstream>
#include <iostream>
#include "Domain.hh"
#include "HTMLRepo.hh"

namespace {
  void writeHTMLPrologue(std::ostream& htmlStream) {
    const std::string htmlPrologue =
R"(<!DOCTYPE html>
<html>
<head>
  <title>Servant Tasks</title>
</head>
<body>
  <table border="1">
    <tr>
      <td>Title</td>
      <td>Type</td>
      <td>Last Performed</td>
      <td>Times Performed</td>
      <td>Vision</td>
    </tr>
)";

    htmlStream << htmlPrologue;
  }

  void writeHTMLEpilogue(std::ostream& htmlStream) {
    const std::string htmlEpilogue =
R"(  </table>
</body>
</html>)";

    htmlStream << htmlEpilogue;
  }

  void writeTaskToHTML(std::ostream& htmlStream, const Task& task) {
    htmlStream
    << "    <tr>\n"
    << "      <td>" << task.title() << "</td>\n"
    << "      <td>" << task.type() << "</td>\n"
    << "      <td>" << task.lastPerformed() << "</td>\n"
    << "      <td>" << task.timesPerformed() << "</td>\n"
    << "      <td>" << task.vision() << "</td>\n"
    << "    </tr>\n";
  }

  Task readTaskFromHTML(const std::vector<std::string>& lines, std::size_t currentIndex) {
    std::vector<std::string> parts;

    for (std::size_t i = currentIndex + 1; i < currentIndex + 6; ++i) {
      auto currentLine = lines[i];

      auto trimmedBeginning = currentLine.substr(10);
      auto trimmedEnd = trimmedBeginning.substr(0, trimmedBeginning.size() - 5);

      parts.push_back(trimmedEnd);
    }

    return taskOfParts(parts);
  }

  std::vector<Task> readTasksFromHTML(std::istream& htmlStream) {
    std::string currentLine;

    for (std::size_t i = 0; i < 14; ++i)
      std::getline(htmlStream, currentLine);

    std::vector<std::string> lines;

    while (std::getline(htmlStream, currentLine))
      lines.push_back(currentLine);

    auto taskCount = (lines.size() - 3) / 7;

    std::vector<Task> tasks;

    for (std::size_t i = 0, currentIndex = 0; i < taskCount; ++i, currentIndex += 7)
      tasks.push_back(readTaskFromHTML(lines, currentIndex));

    return tasks;
  }
}

std::vector<Task> HTMLRepo::_loadData() {
  std::ifstream inFile{_filePath};

  if (inFile.good())
    return readTasksFromHTML(inFile);
  else
    return {};
}

void HTMLRepo::_saveData(const std::vector<Task>& tasks) {
  std::ofstream outFile{_filePath};

  writeHTMLPrologue(outFile);

  for (auto task : tasks)
    writeTaskToHTML(outFile, task);

  writeHTMLEpilogue(outFile);
}

HTMLRepo::HTMLRepo(const std::string& filePath): FileRepo{filePath} { }