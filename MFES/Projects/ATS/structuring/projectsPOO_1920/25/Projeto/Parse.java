
import java.util.*;
import java.time.LocalDateTime;
import java.io.Serializable;
public class Parse implements Serializable 
{
    TrazAqui t = new TrazAqui();
    public void parse(String nomeFicheiro) throws MensagemException{ 
        List<String> linhas = Input.lerFicheiroTexto(nomeFicheiro); 
        String[] linhaPartida;
        for (String linha : linhas) {
                linhaPartida = linha.split(":", 2);
                switch(linhaPartida[0]){
                    case "Utilizador": 
                            Utilizadores u = parseUtilizadores(linhaPartida[1]);
                            //System.out.println(u);
                            try{    
                                    t.registaUtilizador(u);
                                }
                                catch(Exception e1){
                                    throw new MensagemException("Utilizador já está registado. \n");
                                }
                            break;
                            
                    case "Loja": 
                            Lojas l = parseLojas(linhaPartida[1]);
                            //System.out.println(l);
                            try{    
                                    t.registaLoja(l);
                                }
                                catch(Exception e2){
                                    throw new MensagemException("Loja já está registada. \n");
                                }
                            break; 
                            
                    case "Transportadora":
                            EmpresasTransportadoras e = parseEmpresasTransportadoras(linhaPartida[1]);
                            //System.out.println(e);
                            try{    
                                    t.RegistaTransportadora(e);
                                }
                                catch(Exception e3){
                                    throw new MensagemException("Transportadora já está registada. \n");
                                }
                            break; 
                            
                    case "Voluntario":
                            Voluntarios v = parseVoluntarios(linhaPartida[1]);
                            //System.out.println(v);
                            try{    
                                    t.RegistaVoluntario(v);
                                }
                                catch(Exception e4){
                                    throw new MensagemException("Voluntario já está registado. \n");
                                }
                            break; 
                            
                    case "Encomenda":
                            Encomenda enc = parseEncomendas(linhaPartida[1]);
                            //System.out.println(enc);
                            try{    
                                    t.registaEncomenda(enc);
                                }
                                catch(Exception e5){
                                    throw new MensagemException("Encomenda já está registada. \n");
                                }
                            break;
                            
                    case "Aceite":
                            EncomendasAceites aceite = parseAceites(linhaPartida[1]);
                            //System.out.println(aceite);
                            try{    
                                    t.addAceite(aceite);
                                }
                                catch(Exception e5){
                                    throw new MensagemException("Encomenda aceite já está registada. \n");
                                }
                            break;
                            
                    default:
                            System.out.println("Linha inválida.");
                            break;
                }

        }
        System.out.println("Feito!");
    }
    
    /**
     * Método que dá a variável trazAqui lida do ficheiro CSV (utilizado no controlador)
     */
    public TrazAqui getTrazAqui(){ 
        return this.t;
    }
    
    public EmpresasTransportadoras parseEmpresasTransportadoras(String input){
        String[] campos = input.split(",");
        String codTransportadora = campos[0];
        String nomeTransportadora = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        GPS coord = new GPS();
        coord.setX(gpsx);
        coord.setY(gpsy);
        String nif = campos[4];
        double raio = Double.parseDouble(campos[5]);
        double ppKm = Double.parseDouble(campos[6]);
        double numKms = 0.0; 
        boolean apto = false;
        int maxEnc = 1;
        Map<String,Encomenda> filaEspera =  new LinkedHashMap<>();
        List<Integer> avaliacoes = new ArrayList<>();
        Set<Encomenda> registos = new HashSet<>();
        Map<Double,Encomenda> lucro = new HashMap<>(); 
        String password = campos[0];
        return new EmpresasTransportadoras(codTransportadora, nomeTransportadora, coord, nif, raio, ppKm,apto, maxEnc ,filaEspera , avaliacoes , registos, password,lucro,numKms);
    }

    public Voluntarios parseVoluntarios(String input){
        String[] campos = input.split(",");
        String codVoluntario = campos[0];
        String nomeVoluntario = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        GPS coord = new GPS();
        coord.setX(gpsx);
        coord.setY(gpsy);
        double raio = Double.parseDouble(campos[4]);
        boolean disponivel = true;
        boolean apto = false;
        List<Integer> avaliacoes = new ArrayList<>();
        Set<Encomenda> registos = new HashSet<>();   
        String password = campos[0];
        return new Voluntarios (codVoluntario, nomeVoluntario, coord, raio, disponivel, apto,  avaliacoes , registos, password);
    }

    public Utilizadores parseUtilizadores(String input){
        String[] campos = input.split(",");
        String codigo = campos[0]; 
        String nome = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        GPS coord = new GPS();
        coord.setX(gpsx);
        coord.setY(gpsy);
        String email = campos[0] + "@gmail.com";
        String password = campos[0];
        Set<Encomenda> registos = new HashSet<>();  
        return new Utilizadores(nome, codigo, coord, email, password, registos);
    }    

    public LinhaEncomenda parseLinhaEncomenda(String [] input, int i){
        String codP = input[i];
        String descP = input[i+1];
        double quantE = Double.parseDouble(input[i+2]);
        double precoP = Double.parseDouble(input[i+3]);
        int imposto = 0;
        int desconto = 0;
        return new LinhaEncomenda(codP,descP,quantE,precoP,imposto,desconto);
    }    

    public Encomenda parseEncomendas(String input){
        String[] campos = input.split(",");
        String codEncomenda = campos[0];
        String destinatario = campos[1];
        String vendedor = campos[2];
        double peso = Double.parseDouble(campos[3]);
        LocalDateTime data = LocalDateTime.now();
        boolean med = false;
        ArrayList<LinhaEncomenda> encomendas = new ArrayList<>();
        int i = 4;
        while (i < campos.length){
            String codP = campos[i];
            String nome = campos[i+1];
            double quant = Double.parseDouble(campos[i+2]);
            double valor = Double.parseDouble(campos[i+3]);
            int imposto = 0;
            int desconto = 0;
            LinhaEncomenda linha = new LinhaEncomenda(codP,nome,quant,valor,imposto,desconto);
            encomendas.add(linha);
            i += 4;
       }
       return new Encomenda(codEncomenda, destinatario, vendedor, peso, data, med, encomendas);
    }   
    
    public EncomendasAceites parseAceites(String input){
        String[] campos = input.split(",");
        String codEncomenda = campos[0];
        return new EncomendasAceites(codEncomenda);
    }

    public Lojas parseLojas(String input){
        String[] campos = input.split(",");
        String nome = campos[0];
        String codigo = campos[1];
        double gpsx = Double.parseDouble(campos[2]);
        double gpsy = Double.parseDouble(campos[3]);
        GPS coord = new GPS();
        coord.setX(gpsx);
        coord.setY(gpsy);

        Map<String,Encomenda> filaEspera =  new LinkedHashMap<>();
        double tempo = 5;
        String password = campos[0];
  
        return new Lojas (codigo, nome, coord, filaEspera, tempo, password);
    }
}