import java.util.Scanner;
import java.io.ObjectInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.io.File;

public class ProgramController {
    private boolean isExistente;
    private static Menus menus;
    private static String file_path = "../trazaqui10.dat";

    public static String getFile_path() {
        return file_path;
    }

    public ProgramController() {
        this.isExistente = false;
    }

    public boolean isExistente() {
        return isExistente;
    }

    public void setExistente(boolean existente) {
        isExistente = existente;
    }

    public static void main(String[] args) {


        menus = new Menus();
        ProgramController aux = new ProgramController();
        TrazAqui db = null;
        Scanner ler = new Scanner(System.in).useDelimiter("\n");
            do {
                int op = menus.mostraOpcoes("Deseja",
                        new String[]{"Carregar ficheiros default",
                                "Carregar outro ficheiro"});

                if (op == 1) {
                    db = initApp(file_path);
                    //System.out.println(db);
                }

                else if (op == 2) {
                    int opAux = 0;
                    do {
                        System.out.println("Tipo de ficheiro\n1).dat      2).txt       3)Voltar");
                        opAux = ler.nextInt();
                        if(opAux == 1) {
                            System.out.println("Escreva path do ficheiro");
                            String path = ler.next();
                            try {
                                db = initApp(path);
                                opAux=3;
                            }
                            catch(NullPointerException ex) {
                                System.out.println("Erro");
                                opAux=0;
                            }
                            
                        }
                        else if(opAux == 2){
                            System.out.println("Escreva path do ficheiro");
                            String path = ler.next();
                            Parse parse = new Parse(path);
                            parse.parse();
                            
                            try {db = new TrazAqui(parse);
                                opAux=3;}
                            catch(NullPointerException ex) {
                                System.out.println("Erro");
                                opAux=0;
                            }
                            /**
                             * Importante: carregar ficheiro tipo txt aqui
                             */

                        }
                    } while (opAux != 3);

                } else System.exit(0);
            }while (db == null);



      

        while(true) {
            aux.setExistente(false);
            Logger ator = menus.menuLogin(db, aux);
            if (aux.isExistente) {
                if (ator instanceof LogUtilizador) {       //Vai para o menu de cada tipo de classe
                    menus.menuUtilizador(db, ator);
                } else if (ator instanceof LogVoluntario) {
                    menus.menuVoluntario(db, ator);
                }else if (ator instanceof LogEmpresa) {
                    menus.menuTransportadora(db, ator);
                }else if (ator instanceof LogLoja) {
                    menus.menuLoja(db, ator);
                }
            }
        }

    }

        /**
        * Método que inicia o modelo da aplicação TrazAqui.
        */
        private static TrazAqui initApp(String fp) throws NullPointerException{

            TrazAqui traz = new TrazAqui();

        try{FileInputStream fi = new FileInputStream(new File(fp));
            ObjectInputStream oi = new ObjectInputStream(fi);

            traz = (TrazAqui) oi.readObject();
        

            oi.close();
            fi.close();

        } catch (FileNotFoundException e) {
            System.out.println("File not found");
            return  null;
        } catch (IOException e) {
            System.out.println("Error initializing stream");
            return null;
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
            return null;
        }
        return traz;
    }


}