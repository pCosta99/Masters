package View;

import java.util.List;

public interface ITable {
    void addLine(List<String> line);

    void addLines(List<List<String>> lines);

    List<String> toStringList();
}
