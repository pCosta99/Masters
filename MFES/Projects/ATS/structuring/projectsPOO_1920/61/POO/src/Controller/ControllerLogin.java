package Controller;

import Exceptions.ComandoInvalidoException;
import Model.*;

import java.io.IOException;
import java.security.InvalidParameterException;
import java.util.*;
/**
 * Controlador responsável por ações de login e registo
 */
public class ControllerLogin implements Controller {
    private Sistema sys;
    private int NEXT_U = 1;
    private int NEXT_L = 1;
    private int NEXT_V = 1;
    private int NEXT_T = 1;

    /**
     * Construtor de classe
     */
    public ControllerLogin() {
        sys = new Sistema();
    }

    /**
     * Construtor de classe
     *
     * @param sys sistema da aplicação
     */
    public ControllerLogin(Sistema sys) {
        this.sys = sys;
    }

    /**
     * Devolve uma cópia do sistema
     *
     * @return Sistema
     */
    public Sistema getSistema() {
        return sys.clone();
    }

    /**
     * Insere um sistema no controlador
     *
     * @param sys Sistema
     */
    public void setSistema(Sistema sys) {
        this.sys = sys.clone();
    }


    /**
     * Regista um usuário no sistema
     *
     * @param s Lista com a informação inserida pelo utilizador
     * @return null se falhar; mensagem de successo se funcionar;
     */
    public List<String> registo(List<String> s) throws IOException, InvalidParameterException {
        if (s.size() < 4) {
            throw new InvalidParameterException("Registo username password");
        }

        LocalCodeName x;
        String email = s.get(0);
        String password = s.get(1);
        String nome = s.get(2);
        String codigo;
        GPS gps = new GPS();
        gps.randomGPS();
        List<String> ret = new ArrayList<>();

        Login l;
        if (sys.existsEmail(email)) {
            throw new IOException("Username já registado");
        }

        switch (s.get(3).toLowerCase()) {
            case "u":
                codigo = "u" + NEXT_U;
                l = new Login(email, password, codigo);
                x = new Utilizador(codigo, nome, gps, null);
                NEXT_U++;
                break;

            case "l":
                boolean bool = Boolean.parseBoolean(s.get(4));
                codigo = "l" + NEXT_L;
                l = new Login(email, password, codigo);
                x = new Loja(codigo, nome, gps, new PriorityQueue<>(), new PriorityQueue<>(), new TreeMap<>(), bool);
                NEXT_L++;
                break;
            case "v":
                double raio;
                try {
                    raio = Double.parseDouble(s.get(4));
                }catch (Exception e){
                    ret.add("Raio inválido.");
                    return ret;
                }
                codigo = "v" + NEXT_V;
                l = new Login(email, password, codigo);
                x = new Voluntario(codigo, nome, gps, Boolean.parseBoolean(s.get(7)), raio, true, new HashMap<>(), new HashMap<>(), new ArrayList<>(), null);
                NEXT_V++;
                break;

            case "t":
                try {
                    raio = Double.parseDouble(s.get(4));
                }catch (Exception e){
                    ret.add("Raio inválido.");
                    return ret;
                }
                String nif = s.get(5);
                double precKm;
                try {
                    precKm = Double.parseDouble(s.get(6));
                }catch (Exception e){
                    ret.add("Preço inválido.");
                    return ret;
                }
                codigo = "t" + NEXT_T;
                l = new Login(email, password, codigo);
                x = new Transportadora(codigo, nome, Boolean.parseBoolean(s.get(7)), gps, raio, true, new HashMap<>(), new HashMap<>(), new ArrayList<>(), nif, precKm, 0.0, new ArrayList<>(), new ArrayList<>());
                NEXT_T++;
                break;
            default:
                ret.add("Registo inválido.");
                return ret;
        }
        sys.addLogin(l);
        sys.addUser(x);

        ret.add("Registo efetuado com sucesso!");
        ret.add(x.getClass().getSimpleName());
        ret.add(x.getCode());
        return ret;

    }

    /**
     * Faz o login de um interveniente da aplicação
     *
     * @param s informação relativa ao login
     * @return Lista de mensagens de atualização de estado
     * @throws InvalidParameterException
     * @throws IOException
     */
    public List<String> login(List<String> s) throws InvalidParameterException, IOException {
        ArrayList<String> ret = new ArrayList<>();

        if (s.size() != 2) {
            throw new InvalidParameterException("Login id pass");
        }

        if (sys.checksCredentials(s.get(0), s.get(1))) {
            LocalCodeName util = sys.whatIs(s.get(0));
            ret.add("Login efetuado com sucesso!");
            ret.add(util.getClass().getSimpleName());
            ret.add(util.getCode());
        } else {
            throw new IOException("Email ou Password errado/a!");
        }

        return ret;
    }

    /**
     * Executa um comando no sistema
     *
     * @param s informação relativa ao que se pretende executar
     * @return Lista de strings com atualização de estado ou outras informações relevantes
     * @throws IOException              Erro de email ou password
     * @throws ComandoInvalidoException Comando Inválido
     */
    public List<String> execute(List<String> s) throws IOException, ComandoInvalidoException {
        List<String> ret = new ArrayList<>();
        List<String> aux = s.subList(1, s.size());
        switch (s.get(0)) {
            case "registo":
                ret = registo(aux);
                break;
            case "login":
                ret = login(aux);
                break;
            case "load":
                try {
                    sys = EscritaLeitura.readData();
                    ret.add("Ficheiros carregados!");
                } catch (IOException | ClassNotFoundException e) {
                    ret.add("Houve algum problema no carregamento de dados.");
                }

                break;
            case "save":
                try {
                    EscritaLeitura.saveData(sys);
                    ret.add("Ficheiros guardados!");
                }catch (IOException e){
                    ret.add("Não foi possível guardar o estado atual.");
                }
                break;
            default:
                throw new ComandoInvalidoException("Comando Inválido!");
        }
        return ret;
    }


}

