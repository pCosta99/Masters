/**
 * Classe que guarda e carrega dados
 */
package Files;

import Model.GestTrazAqui;

import java.io.*;

public class GuardarCarregarEstado {

    /**
     * Método que guarda os dados num ficheiro
     *
     * @param fileName  nome ficheiro
     * @param c         GestTrazAqui
     * @return          0 se guardou sem erros
     */
    public int guardaDados(String fileName, GestTrazAqui c) {
        try {
            FileOutputStream file = new FileOutputStream(fileName);
            ObjectOutputStream oos = new ObjectOutputStream(file);
            oos.writeObject(c);
            oos.flush();
            oos.close();
        }
        catch (FileNotFoundException e) {
            return 1;
        }
        catch (IOException e) {
            return 2;
        }

        return 0;
    }

    /**
     * Método que carrega os dados guardados num ficheiro
     *
     * @param fileName                  nome ficheiro
     * @return                          estrutura carregada
     * @throws IOException              controlo erros
     * @throws ClassNotFoundException   controlo erros
     */
    public GestTrazAqui carregaDados(String fileName) throws IOException, ClassNotFoundException {
        FileInputStream file = new FileInputStream(fileName);
        ObjectInputStream ois = new ObjectInputStream(file);
        GestTrazAqui c = (GestTrazAqui) ois.readObject();
        ois.close();
        return c;
    }
}
