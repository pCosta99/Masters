package TrazAqui;

import java.io.*;
import java.util.HashMap;
import java.util.Map;

/**
 * Classe responsável pelo controlo sobre os ficheiros necessários para o programa.
 */
public class FileIO {
    private String readLogPath;
    private String savedPath;
    private String accPath;

    /**
     * Construtor vazio de FileIO.
     */
    public FileIO() {
        this.readLogPath = "";
        this.accPath = "";
        this.savedPath = "";
    }

    /**
     * Construtor parametrizado de FileIO.
     * @param p Path do ficheiro de logs.
     * @param p2 Path do ficheiro do estado.
     * @param p3 Path do ficheiro de credenciais.
     */
    public FileIO(String p, String p2, String p3) {
        this.readLogPath = p;
        this.savedPath = p2;
        this.accPath = p3;
    }

    /**
     * Construtor por cópia de FileIO.
     * @param f FileIO que pretendemos copiar.
     */
    public FileIO(FileIO f) {
        this.readLogPath = f.getReadLogPath();
        this.savedPath = f.getSavedPath();
        this.accPath = f.getAccPath();
    }

    /**
     * Getter do path do ficheiro de estado.
     * @return Path do ficheiro do estado.
     */
    public String getSavedPath() {
        return savedPath;
    }

    /**
     * Setter do path do ficheiro de estado.
     * @param savedPath Path do ficheiro de estado.
     */
    public void setSavedPath(String savedPath) {
        this.savedPath = savedPath;
    }

    /**
     * Getter do path do ficheiro de credenciais.
     * @return Path do ficheiro de credenciais.
     */
    public String getAccPath() {
        return accPath;
    }

    /**
     * Setter do path do ficheiro de credenciais.
     * @param accPath Path do ficheiro de credenciais.
     */
    public void setAccPath(String accPath) {
        this.accPath = accPath;
    }

    /**
     * Getter do path do ficheiro de logs.
     * @return Path do ficheiro de logs.
     */
    public String getReadLogPath() {
        return readLogPath;
    }

    /**
     * Setter do path do ficheiro de logs.
     * @param readLogPath Path do ficheiro de logs.
     */
    public void setReadLogPath(String readLogPath) {
        this.readLogPath = readLogPath;
    }

    /**
     * Método que lê de um ficheiro.
     * @param e Estado.
     * @throws IOException Caso hajam erros de IO, dá excecão.
     * @throws LojaInexistenteException Caso a loja não exista no sistema, dá exceção.
     */
    public void loadFromFile(Estado e) throws IOException, LojaInexistenteException {
        BufferedReader file;
        GPS gps;
        file = new BufferedReader(new FileReader(this.readLogPath));
        String line = file.readLine(), temp;
        String[] tokens;
        Map<String,Encomenda> buffer = new HashMap<>();
        int start = 42;
        int i = 0;
        while(line!=null) {
            if(i > start) {
                tokens = line.split(":");
                temp = tokens[0];
                tokens = tokens[1].split(",");
                switch(temp) {
                    case "Utilizador":
                        gps = new GPS(Double.parseDouble(tokens[2]),Double.parseDouble(tokens[3]));
                        Utilizador u = new Utilizador(tokens[1],tokens[0],gps,new HashMap<>());
                        try {
                            e.addUtilizador(u);
                        } catch (ExistingCodeException ex) {
                            UI.print(ex.getMessage());
                        }
                        break;
                    case "Voluntario":
                        gps = new GPS(Double.parseDouble(tokens[2]),Double.parseDouble(tokens[3]));
                        Voluntario v = new Voluntario();
                        v.setRaio(Double.parseDouble(tokens[4]));
                        v.setNome(tokens[1]);
                        v.setCod(tokens[0]);
                        v.setLocalizacao(gps);
                        try {
                            e.addTrabalhador(v);
                        }
                        catch (ExistingCodeException ex) {
                            UI.print(ex.getMessage());
                        }
                        break;
                    case "Transportadora":
                        gps = new GPS(Double.parseDouble(tokens[2]),Double.parseDouble(tokens[3]));
                        Transportadora t = new Transportadora();
                        t.setCod(tokens[0]);
                        t.setNome(tokens[1]);
                        t.setLocalizacao(gps);
                        t.setRaio(Double.parseDouble(tokens[5]));
                        t.setPrecoKM(Double.parseDouble(tokens[6]));
                        try {
                            e.addTrabalhador(t);
                        } catch (ExistingCodeException ex) {
                            UI.print(ex.getMessage());
                        }
                        break;
                    case "Loja":
                        gps = new GPS(Double.parseDouble(tokens[2]),Double.parseDouble(tokens[3]));
                        Loja j = new Loja();
                        j.setCod(tokens[0]);
                        j.setNome(tokens[1]);
                        j.setLocalizacao(gps);
                        try {
                            e.addLoja(j);
                        } catch (ExistingCodeException ex) {
                            UI.print(ex.getMessage());
                        }
                        break;
                    case "Encomenda":
                        Encomenda s = new Encomenda();
                        s.setCod(tokens[0]);
                        s.setUtilizador(tokens[1]);
                        s.setLoja(tokens[2]);
                        s.setPeso(Double.parseDouble(tokens[3]));
                        LinhaEncomenda linha = new LinhaEncomenda();
                        for(int ind = 4; ind < tokens.length; ind+=4) {
                            linha.setCod(tokens[ind]);
                            linha.setDescricao(tokens[1+ind]);
                            linha.setQuantidade(Double.parseDouble(tokens[2+ind]));
                            linha.setPreco(Double.parseDouble(tokens[3+ind]));
                            s.addProduto(linha);
                        }
                        buffer.put(s.getCod(),s);
                        break;
                    case "Aceite":
                        Encomenda enc = buffer.get(tokens[0]).clone();
                        buffer.remove(tokens[0]);
                        e.addEncomendaUtilizador(enc.getUtilizador(),enc);
                        break;
                    default:
                        break;
                }
            } else i++;
            line = file.readLine();
        }

        if (!buffer.isEmpty()) {
            for (Map.Entry<String, Encomenda> aux : buffer.entrySet()) {
                    e.addEncomendaLoja(aux.getValue().getLoja(), aux.getValue());
            }
        }
        file.close();
    }

    /**
     * Método que guarda em ficheiro.
     * @param e Estado de onde pretendemos obter informação para guardar em ficheiro.
     * @throws IOException Caso hajam erros de IO, dá exceção.
     */
    public void saveToFile(Estado e) throws IOException {
        BufferedWriter file = new BufferedWriter(new FileWriter(this.savedPath));
        for(Utilizador u: e.getUtilizadores().values())
            file.write("Utilizador:"+u.getCod()+","+u.getNome()+","+u.getLocalizacao().getLatitude()+","+u.getLocalizacao().getLongitude()+"\n");

        for(Estafeta u: e.getTrabalhadores().values()) {
            if (u instanceof Voluntario) {
                Voluntario v = (Voluntario) u;
                file.write("Voluntario:"+v.getCod()+","+v.getNome()+","+v.getLocalizacao().getLatitude()+","+v.getLocalizacao().getLongitude()+","+v.getRaio()+"\n");
            } else if (u instanceof Transportadora) {
                Transportadora t = (Transportadora) u;
                file.write("Transportadora:"+t.getCod()+","+t.getNome()+","+t.getLocalizacao().getLatitude()+","+t.getLocalizacao().getLongitude()+","+t.getNIF()+","+u.getRaio()+","+t.getPrecoKM()+"\n");
            }
        }

        for(Loja u: e.getLojas().values())
            file.write("Loja:"+u.getCod()+","+u.getNome()+","+u.getLocalizacao().getLatitude()+","+u.getLocalizacao().getLongitude()+"\n");

        for(Loja u: e.getLojas().values())
            for(Encomenda enc: u.getPedidos()) {
                write(file, enc);
            }

        for (Utilizador u : e.getUtilizadores().values()) {
            for (Encomenda enc : u.getEncomendasConcluidas().values()) {
                write(file, enc);
            }
        }

        for (Estafeta u : e.getTrabalhadores().values()) {
            for (Encomenda enc : u.getEncomendasEntregues()) {
                write(file, enc);
            }
            for (Encomenda enc : u.getPedidosEncomenda()) {
                write(file, enc);
            }
        }

        for (Utilizador u : e.getUtilizadores().values()) {
            for (Encomenda enc : u.getEncomendasConcluidas().values()) {
                file.write("Aceite:" + enc.getCod());
                file.newLine();
            }
        }
        file.close();
    }

    /**
     * Escreve em ficheiro.
     * @param file Ficheiro para onde vamos escrever.
     * @param enc Encomenda.
     * @throws IOException Caso hajam erros de IO, dá exceção.
     */
    private void write(BufferedWriter file, Encomenda enc) throws IOException {
        file.write("Encomenda:"+enc.getCod()+","+enc.getUtilizador()+","+enc.getLoja()+","+enc.getPeso());
        for(LinhaEncomenda linha: enc.getProdutos())
            file.write(","+linha.getCod()+","+linha.getDescricao()+","+linha.getQuantidade()+","+linha.getPreco());
        file.newLine();
    }

    /**
     * Regista uma conta em ficheiro.
     * @param email Email do utilizador.
     * @param password Password do utilizador.
     * @param ent Dados do login do utilizador.
     * @param e Estado.
     * @throws IOException Caso hajam erros de IO, dá exceção.
     * @throws ExistingCodeException Caso já exista um utilizador com o mesmo código, dá exceção.
     */
    public void registaConta(String email, String password, Entrada ent, Estado e) throws IOException, ExistingCodeException {
        FileWriter fw = new FileWriter(this.accPath,true);
        BufferedWriter writer = new BufferedWriter(fw);
        writer.write(email + "," + password + "," + ent.getCod() + "," + ent.toStringNome() + "\n");
        writer.flush();
        writer.close();
        fw.close();
        e.add(ent);
    }

    /**
     * Método que compara os dados inseridos para login com os dados existentes em ficheiro.
     * @param email Email inserido pelo utilizador.
     * @param pass Password inserida pelo utilizador.
     * @param e Estado do sistema.
     * @throws IOException Caso hajam erros de IO, dá exceção.
     * @throws InvalidInputException Caso o input dado não seja válido, dá exceção.
     */
    public void validaLogin(String email, String pass, Estado e) throws IOException, InvalidInputException {
        boolean found = false;
        String cod = "";
        FileReader file = new FileReader(this.accPath);
        BufferedReader reader = new BufferedReader(file);
        String data;
        String[] tok = new String[0];
        while ((data = reader.readLine())!=null && !found) {
            tok = data.split(",");
                if (tok[0].equals(email) && tok[1].equals(pass)) {
                    found = true;
                    cod = tok[2];
                }
            }
        if(found) {
            switch(tok[3]) {
                case "Utilizador":
                    e.setLogin(e.getUtilizador(cod));
                    break;
                case "Loja":
                case "LojaFilaEspera":
                    e.setLogin(e.getLoja(cod));
                    break;
                case "Voluntario":
                case "Transportadora":
                    e.setLogin(e.getEstafeta(cod));
                    break;
                default:
                    break;
            }
        }
        else throw new InvalidInputException("Email ou password inválido");
        file.close();
        reader.close();
    }

    /**
     * Guarda um Estado em modo binário.
     * @param e Estado que pretendemos guardar.
     * @throws IOException Caso hajam erros de IO, dá exceção.
     */
    public void saveObjectStream(Estado e) throws IOException {
        FileOutputStream fos = new FileOutputStream(this.savedPath);
        ObjectOutputStream oos = new ObjectOutputStream(fos);
        oos.writeObject(e);
        oos.flush();
        oos.close();
        fos.close();
    }

    /**
     * Lê um estado guardado em modo binário.
     * @return Estado lido do modo binário.
     * @throws IOException Caso hajam erros de IO, dá exceção.
     * @throws ClassNotFoundException Caso a classe não seja encontrada, dá exceção.
     */
    public Estado readObjectStream() throws IOException, ClassNotFoundException {
        FileInputStream fis = new FileInputStream(this.savedPath);
        ObjectInputStream ois = new ObjectInputStream(fis);
        Estado e;
        e = (Estado) ois.readObject();
        ois.close();
        fis.close();
        return e;
    }


}
