package Model.Leitura;


import Model.ISistema;

import java.io.*;

/**
 * Classe responsável pela leitura  e por guardar o estado do programa num ficheiro .dat:
 * fileOut -> nome do ficheiro que vai guardar o estado.
 * fileIn -> nome do ficheiro que carrega um estado.
 */
public class RWEstado implements IRWEstado {
    private String fileOut;
    private String fileIn;


    public RWEstado(){
        this.fileIn = "sistemaLog.dat";
        this.fileOut = "sistemaLog.dat";
    }

    public void setFileOut(String file1) {
        this.fileOut = file1;
    }

    public void setFileIn(String file2) {
        this.fileIn = file2;
    }

    /**
     *Guarda o estado de um programa.
     */
    public void saveData (ISistema sistema) throws IOException {
        ObjectOutputStream f = new ObjectOutputStream(new FileOutputStream(fileOut));
        f.writeObject(sistema);
        f.close();
    }

    /**
     * Carrega um estado do programa.
     */
    public ISistema loadData() throws IOException, ClassNotFoundException {
        ObjectInputStream f = new ObjectInputStream(new FileInputStream(fileIn));
        ISistema sgv = (ISistema) f.readObject();
        f.close();
        return sgv;
    }
}