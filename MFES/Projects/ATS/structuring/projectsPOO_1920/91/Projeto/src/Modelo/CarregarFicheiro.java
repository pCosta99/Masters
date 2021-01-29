package Modelo;

import Visualizador.Visualizador;

import java.io.*;


public class CarregarFicheiro {
    /**
     * Guarda os dados num ficheiro
     * @param nome  Nome do ficheiro
     * @param sge   Sistema de gestão de entregas
     */
    public void guardar ( String nome , SistemaGestaoEntregas sge){
        try{
            FileOutputStream ficheiro =new FileOutputStream(nome);
            ObjectOutputStream novo = new ObjectOutputStream(ficheiro);
            novo.writeObject(sge);
            novo.flush();
            novo.close();
        } catch (FileNotFoundException e) {
            Visualizador.printString(e.getMessage());
        } catch (IOException e) {
            Visualizador.printString(e.getMessage());
        }
    }

    /**
     * Carrega os dados guardados num ficheiro
     * @param nome   Nome do ficheiro
     */
    public SistemaGestaoEntregas carregar(String nome) {
        try{
            FileInputStream ficheiro = new FileInputStream(nome);
            ObjectInputStream novo = new ObjectInputStream(ficheiro);
            SistemaGestaoEntregas sge = (SistemaGestaoEntregas) novo.readObject();
            novo.close();
            return sge;
        } catch (FileNotFoundException e) {
            e.printStackTrace();
        } catch (IOException e) {
            Visualizador.printString(e.getMessage());
        } catch (ClassNotFoundException e) {
            Visualizador.printString(e.getMessage());
        } return null;
    }
}
