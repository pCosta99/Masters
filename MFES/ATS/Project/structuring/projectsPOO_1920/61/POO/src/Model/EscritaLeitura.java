package Model;

import java.io.*;

/**
 * Classe que coordena a transmissão de dados entre o ficheiros e o programa
 */
public class EscritaLeitura {

    /**
     * Guarda os dados do programa num ficheiro
     *
     * @param sys A estrutura onde estão guardados os dados do programa
     * @throws IOException ???
     */
    public static void saveData(Sistema sys) throws IOException {
        File newFile = new File("data/dados");
        FileOutputStream fos = new FileOutputStream(newFile);
        ObjectOutputStream oos = new ObjectOutputStream(fos);
        oos.writeObject(sys);
        oos.flush();
        oos.close();
    }

    /**
     * Lê os dados de um ficheiro e carrega-os no programa
     *
     * @return A estrutura onde foram guardados os dados do ficheiro
     * @throws IOException            ???
     * @throws ClassNotFoundException ???
     */
    public static Sistema readData() throws IOException, ClassNotFoundException {
        File readFile = new File("data/dados");
        FileInputStream fis = new FileInputStream(readFile);
        ObjectInputStream ois = new ObjectInputStream(fis);
        Sistema sys;
        sys = (Sistema) ois.readObject();
        ois.close();

        return sys;
    }
}
