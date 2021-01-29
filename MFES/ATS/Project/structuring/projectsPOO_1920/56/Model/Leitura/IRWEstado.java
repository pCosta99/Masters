package Model.Leitura;

import Model.ISistema;

import java.io.IOException;

public interface IRWEstado {
    void setFileOut(String file1);
    void setFileIn(String file2);
    void saveData(ISistema sistema) throws IOException;
    ISistema loadData() throws IOException, ClassNotFoundException;
}
