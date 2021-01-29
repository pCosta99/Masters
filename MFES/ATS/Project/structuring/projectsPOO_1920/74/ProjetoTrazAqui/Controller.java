import java.util.Scanner;
import java.util.HashMap;
import java.util.TreeSet;
import java.util.ArrayList;
import java.util.Map;
import java.time.LocalDateTime;
import java.util.List;
import java.util.LinkedList;
import java.util.AbstractMap.SimpleEntry;
import java.util.LinkedHashMap;
import java.io.Serializable;
import java.io.FileOutputStream;
import java.io.ObjectOutputStream;
import java.io.FileInputStream;
import java.io.ObjectInputStream;
import java.io.IOException;
import java.io.FileNotFoundException;
import java.util.function.Consumer;
import java.util.stream.Stream;
import java.util.stream.Collectors;
public class Controller implements IController, Serializable{
	private ISistema model;
	private IViewer viewer;
	/**
     * Construtor por omissao da classe
     */
	public Controller(){
		this.model = new Sistema();
	}
	/**
     * Construtor parametrizado da classe que aceita como parametro o objecto , que implementa a interface ISistema
     */
	public Controller(ISistema sistema){
		this.model = sistema;
	}
	/**
     * Construtor parametrizado da classe que aceita como parametro o objecto , que implementa a interface ISistema
     * e um que implementa a interface IViewer
     */
	public Controller(ISistema sistema, IViewer viewer){
		this.model = sistema;
		this.viewer = viewer;
	}
	/**
     * Construtor de copia da classe  
     * Aceita como parametro outro objecto da classe e utiliza os metodos
     * de acesso aos valores das variaveis de instancia
     */ 
	public Controller(Controller c){
		this.model = c.getModel();
		this.viewer = c.getViewer();
	}
	 /**
     * Devolve o objecto que implementa a interface ISistema
     * @return objecto que implementa a interface ISistema
     */
	public ISistema getModel(){
		return this.model;
	}
	/**
     * Devolve o objecto que implementa a interface IViewer
     * @return objecto que implementa a interface IViewer
     */
	public IViewer getViewer(){
		return this.viewer;
	}
	/**
     * Método que verifica se dois obectos da classe são iguais
     * @return boolean resulante da comparaçao
     */
	public boolean equals(Object o){
		if(o == this) return true;
		if(o == null || (o.getClass() != this.getClass())) return false;

		Controller c = (Controller) o;

		return c.getModel().equals(this.model);
	}
	/**
     * Faz clone do objecto da classe
     * @return  objecto clonizada
     */
	public Controller clone(){
		return new Controller(this);
	}
	/**
     * Devolve em forma de String o objecto da classe
     * @return A String do objecto da classe
     */
	public String toString(){
		StringBuilder sb = new StringBuilder();
		sb.append(this.model);
		sb.append(this.viewer);
		return sb.toString();
	}
	/**
     * Atualiza o objecto ,que implementa a interface IViewer, da classe
     * @param v novo viewer da classe
     */
	public void setViewer(IViewer v){
		this.viewer = v;
	}
	/**
     * Atualiza o objecto ,que implementa a interface ISistema, da classe
     * @param v novo model da classe
     */
	public void setModel(ISistema v){
		this.model = v;
	}

    public boolean regista(String entidade, String [] op){
        boolean ret = false;
        try{

            Coordenada gps = new Coordenada(Double.parseDouble(op[op.length-2]),Double.parseDouble(op[op.length-1]));
        switch(entidade){
            case "u":
                try{
                    Utilizador u = new Utilizador(op[1],op[0],op[2],op[3],gps,new TreeSet<>());
                    ret = this.model.adicionaEntidade(u);
                    
                    
                }catch(AddEntidadeRepetidaException e){
                    this.viewer.showMessage(e.getMessage());
                }
                break;
            case "l":
                try{
                    Loja l = new Loja(op[1],op[0],gps,op[2],op[3], new ArrayList<>(), false);
                    ret = this.model.adicionaEntidade(l);
                    
                }catch(AddEntidadeRepetidaException e){
                    this.viewer.showMessage(e.getMessage());
                }
                break;
            case "v":
                try{
                    if(op[4].equals("1")){
                        Voluntario v = new VoluntarioMedico(op[0],op[1],op[2],op[3],gps,Double.parseDouble(op[4]),true);
                        ret = this.model.adicionaEntidade(v);
                        
                    }else if(op[4].equals("2")){
                    	Voluntario v = new Voluntario(op[0],op[1],op[2],op[3],gps,Double.parseDouble(op[4]));
                        
                    	ret = this.model.adicionaEntidade(v);
                    }else
                        this.viewer.showMessage("Valores inválidos de registo.");
                }catch(AddEntidadeRepetidaException e){
                    this.viewer.showMessage(e.getMessage());
                }
                break;
            case "t":
                try{
                    if(op[5].equals("1")){
                        Transportadora t = new TransportadoraMedica(op[1],op[0],gps,op[2],op[3],Integer.parseInt(op[4]),50,op[7],Double.parseDouble(op[6]),Double.parseDouble(op[8]),new ArrayList<>(),new HashMap<>(),true);
                        ret = this.model.adicionaEntidade(t);
                        
                        
                    }else if(op[5].equals("2")){
                        Transportadora t = new Transportadora(op[1],op[0],gps,op[2],op[3],Integer.parseInt(op[4]),50,op[7],Double.parseDouble(op[6]),Double.parseDouble(op[8]),new ArrayList<>(),new HashMap<>());
                        ret = this.model.adicionaEntidade(t);	
                        
                        
                    }else
                        this.viewer.showMessage("Valores inválidos de registo.");
                }catch(AddEntidadeRepetidaException e){
                    this.viewer.showMessage(e.getMessage());
                }
                break;
        }
        }catch(NumberFormatException e){
            this.viewer.showMessage("Valores de registo inválidos.");
        }
        return ret;
    }
    
     /**
     * Método que verifica se as credenciais colocadas no registo sao validas ou nao
     * @param String entidade,String email,String pass
     * @return String da entidade resultante
     */
	public Entidade verificaCredenciais(String email,String pass){
		Entidade ret = null;
		try{
			ret = this.model.validaCredenciaisEntidades(email,pass);
        }catch(CredenciaisErradasException e){
			this.viewer.showMessage(e.getMessage());
		}catch(EntidadeNaoExistenteException f){
            this.viewer.showMessage(f.getMessage());
        }
		return ret;
	}
	/**
     * Método que verifca se um dado codigo representa alguma entidade do sistema
     * @param String cod
     * @return bolean resultante da verificaçao
     */
	public boolean validaCodigo(String cod){
		return this.model.existeCodigo(cod);
	}
	 /**
     * Método que recebendo um dado codigo correspondente a uma entidade cria uma List com todos os objectos 
     * da classe RegistosTransporte existentes na entidade
     * @param String codigo
     * @return List<RegistosTransporte>
     */
	public List<RegistosTransporte> registosTransporte(String codigo){ // Alterar isto
		List<RegistosTransporte> reg = null;
		try{
			reg = this.model.registosTransporte(codigo);
		}catch(EntidadeNaoExistenteException e){
			this.viewer.showMessage(e.getMessage());
		}
		return reg;
	}
	/**
     * Método que cria uma List<String> com os codigos das lojas existentes no sistema
     * @return List<String>
     */
	public List<String> getListaLojas(){
		//return this.model.getCodigoLojas();
        return this.model.getListLojas();
	}

    private void atualizaEntidade(String codEntity, String op,String tipo){
        
        if(op.equals("1") || op.equals("2") || op.equals("3"))
            this.atualizaPerfilGeral(codEntity,op);
        else if((op.equals("4") || op.equals("5")) && (tipo.equals("T") || tipo.equals("V")))
            this.atualizaPerfilTransporte(codEntity,op);
        else if((op.equals("4") || op.equals("5")) && tipo.equals("L"))
            this.atualizaPerfilLoja(codEntity,op);
        else if(op.equals("6") && tipo.equals("TM") || tipo.equals("VM"))
            this.atualizaPerfilTransporteMedico(codEntity,op);

    }
    /**
     * Metodo utilizado para alterar os dados de perfil de qualquer Entidade do Sistema.
     * @param String codEntity, String tipo
     */ 
    private void atualizaPerfilGeral(String codEntity,String op){
        String novo = "";
        switch(op){
            case "1": //altera o nome
                this.viewer.showMessage("Insira o novo nome.");
                this.viewer.showInputLine("Nome");
                novo = this.viewer.recebeInput();
                try{    
                    String aux1 = novo;
                    Consumer<Entidade> alteraEntidade = h -> h.setNome(aux1);
                    this.model.atualizaEntidade(codEntity,alteraEntidade);
                }
                catch(EntidadeNaoExistenteException exc){
                    this.viewer.showMessage("A entidade não existe.");
                }
                break;
            case "2": //altera a password
                this.viewer.showMessage("Insira a nova password.");
                this.viewer.showInputLine("Password");
                novo = this.viewer.recebeInput();
                try{
                    String aux2 = novo;
                    Consumer<Entidade> alteraPassword = h -> h.setPassword(aux2);
                    this.model.atualizaEntidade(codEntity,alteraPassword);
                }
                catch(EntidadeNaoExistenteException exc){
                    this.viewer.showMessage("A entidade não existe.");
                }
                break;
            case "3": //altera o email
                this.viewer.showMessage("Insira o novo email.");
                this.viewer.showInputLine("Email");
                novo = this.viewer.recebeInput();
                try{
                    String aux3 = novo;
                    if(!this.model.checkEmail(novo)){
                        Consumer<Utilizador> alteraEmail = h -> h.setEmail(aux3);
                        this.model.atualizaUtilizador(codEntity,alteraEmail); 
                    }else
                        this.viewer.showMessage("Email já existente!");
                }
                catch(EntidadeNaoExistenteException exc){
                    this.viewer.showMessage("A entidade não existe.");
                }
                break;
        }
    }

    private void atualizaPerfilLoja(String codEntity,String op){
        String novo = null;
        
        switch(op){
            case "4": //altera o nome
                this.viewer.showMessage("Insira o novo tamanho.");
                this.viewer.showInputLine("Tamanho");
                String novotam = this.viewer.recebeInput();
                try{    
                    int tamanho = Integer.parseInt(novotam);
                    if(tamanho >= 0){
                        Consumer<Loja> alteraTamanho = h -> h.setTamFila(tamanho);
                        this.model.atualizaLoja(codEntity,alteraTamanho);
                    }else this.viewer.showMessage("Valor inválido para o tamanho.");
                }
                catch(EntidadeNaoExistenteException exc){
                    this.viewer.showMessage(exc.getMessage());
                }catch(NumberFormatException tam){
                    this.viewer.showMessage("Valor inválido para o tamanho.");
                }
                break;
            case "5": //altera a password
                this.viewer.showMessage("Insira o novo tempo.");
                this.viewer.showInputLine("tempo");
                String novotempo = this.viewer.recebeInput();
                try{    
                    double tempo = Double.parseDouble(novotempo);
                    if(tempo >= 0){
                        Consumer<Loja> alteraTempo = h -> h.setTempoMedio(tempo);
                        this.model.atualizaLoja(codEntity,alteraTempo);
                    }else this.viewer.showMessage("Valor inválido para o tempo");
                }
                catch(EntidadeNaoExistenteException exc){
                    this.viewer.showMessage(exc.getMessage());
                }catch(NumberFormatException temp){
                    this.viewer.showMessage("Valor inválido para o tempo.");
                }
                break;
        }
    }


    private void atualizaPerfilTransporte(String codEntity,String op){
        String novo = null;
        switch(op){
            case "4":
                this.viewer.showMessage("Insira o novo valor da velocidade.");
                this.viewer.showInputLine("Velocidade");
                String novaVelocidade = this.viewer.recebeInput();
                try{    
                    double velocidade = Double.parseDouble(novaVelocidade);
                    if(velocidade > 0){
                        Consumer<ITransporte> alteraVelocidade = h -> h.setVelocidade(velocidade);
                        this.model.atualizaTransporte(codEntity,alteraVelocidade);
                    }else this.viewer.showMessage("Valor inválido para a velocidade.");
                }
                catch(EntidadeNaoExistenteException exc){
                    this.viewer.showMessage(exc.getMessage());
                }catch(NumberFormatException tam){
                    this.viewer.showMessage("Valor inválido para a velocidade.");
                }
                break;
            case "5":
                try{
                    Entidade aux = this.model.getEntidade(codEntity);
                    boolean livre = false;
                    if(aux instanceof ITransporte){
                        ITransporte t = (ITransporte) aux;
                        
                        boolean livreF = !t.getLivre();
                        Consumer<ITransporte> change = a -> a.setLivre(livreF);
                        this.model.atualizaTransporte(codEntity, change);
                        livre = livreF;
                    }
                    if(livre)
                        this.viewer.showMessage("Ficou disponível.");
                    else
                        this.viewer.showMessage("Ficou indisponível.");
                }catch(EntidadeNaoExistenteException e){
                    this.viewer.showMessage(e.getMessage());
                }
                break;
        }
    }


    private void atualizaPerfilTransporteMedico(String codEntity,String op){
        String novo = null;
        
        if(op.equals("6")){
            try{
                Entidade aux = this.model.getEntidade(codEntity);
                boolean aceita = false;
                if(aux instanceof ITransporteMedico){
                    ITransporteMedico t = (ITransporteMedico) aux;
                    boolean aceitaF = !t.aceitoTransporteMedicamentos();
                    Consumer<ITransporteMedico> change = a -> a.aceitaMedicamentos(aceitaF);
                    this.model.atualizaTransporteMedico(codEntity, change);
                    aceita = aceitaF;
                }
                if(aceita)
                    this.viewer.showMessage("Ficou disponível para transportar encomendas médicas.");
                else
                    this.viewer.showMessage("Ficou indisponível para transportar encomendas médicas.");
            }catch(EntidadeNaoExistenteException e){
                this.viewer.showMessage(e.getMessage());
            }
        }
    }
     /**
     * Método que executa todas as opçoes que um Utilizador do sistema tem acesso
     * @param String [] opcao, String codEntity
     */
    public void opcoesUtilizador(String [] opcao, String codEntity, String tipo){
        switch(opcao[0]){
            case "1":
                Encomenda e = this.viewer.fazerEncomenda();
                if(e != null) e.setCodUtilizador(codEntity);
                if(e != null && e.getSize() > 0) {
                    try{
                        e.setCodUtilizador(codEntity);
                        String ret = this.model.fazPedido(e,new ArrayList<>());
                        if(ret.equals(""))
                            this.viewer.showMessage("De momento não há meios para transportar a encomenda " + e.getCodEncomenda() + ".");
                        else
                            this.viewer.showMessage("A encomenda " + e.getCodEncomenda() + " foi atribuiada a " + ret + ".");
                    }catch(EntidadeNaoExistenteException f){
                        this.viewer.showMessage(f.getMessage());
                    }catch(TransporteNaoExistenteException x){
                        this.viewer.showMessage(x.getMessage());
                    }
                }else
                    this.viewer.showMessage("Ocorreu um erro com o pedido.");
                break;
            case "2":
                List<String> lista = this.model.encomendasNoSistema(codEntity).stream().map(Encomenda::codigos).collect(Collectors.toList());
                if(!lista.isEmpty()){
                    this.viewer.showList(lista,3);
                    this.viewer.showInputLine("Codigo");
                    String codigo = this.viewer.recebeInput();
                    try{
                        int status = this.model.estadoDeEncomenda(codEntity, codigo);
                        if(status == 1)
                            this.viewer.showMessage("A encomenda está pendente da aceitação do utilizador.");
                        else if(status == 2)
                            this.viewer.showMessage("A encomenda está pendente da aceitação do transporte. ");
                        else if(status == 3)
                            this.viewer.showMessage("A encomenda está à espera de ser processada pela loja. ");
                        else if(status == 4)
                            this.viewer.showMessage("A encomenda está pronta a ser entregue");
                        break;
                    }catch(PedidoNaoExistenteException x){
                        this.viewer.showMessage(x.getMessage());
                    }
                }else
                    this.viewer.showMessage("Ainda não fez qualquer encomenda!");
                break;
            case "3":
                try{
                    List<Pedido> ret = this.model.listaDeEncomendasPendentesUtilizador(codEntity);
                    List<String> aux = new ArrayList<>();
                    for(Pedido enc : ret){
                        StringBuilder enP = new StringBuilder();
                        double rating = this.model.getRating(enc.getTransporte());
                        enP.append(enc.info()).append("Rating: ").append(rating).append("\n");
                        aux.add(enP.toString());
                    } 
                    if(aux.size() > 0){
                        this.viewer.showList(aux,1);
                        SimpleEntry<String,String> cod = this.viewer.escolherEncomendaPendenteUtilizador(ret);
                        if(cod.getKey().equals("1"))
                            this.model.utilizadorAceitaEncomenda(codEntity, cod.getValue());
                        else if(cod.getKey().equals("2")){
                            String codEnc = this.model.utilizadorRejeitaEncomenda(codEntity, cod.getValue());
                            this.viewer.showMessage("O serviço proposto por " + cod.getValue() + " foi rejeitado.\nA encomenda foi atribuida a " + codEnc);
                        }
                    }else
                        this.viewer.showMessage("Não há encomendas");
                }catch(EntidadeNaoExistenteException f){
                    this.viewer.showMessage(f.getMessage());
                }catch(TransporteNaoExistenteException x){
                    this.viewer.showMessage(x.getMessage());
                }catch(PedidoNaoExistenteException pe){
                    this.viewer.showMessage(pe.getMessage());
                }
                break;    
            case "4":
                SimpleEntry<String,String> classificacao = this.viewer.inputClassificar();
                try{
                    int valor = Integer.parseInt(classificacao.getValue());
                    if(!classificacao.getKey().equals("0") && valor <= 10 && valor >=0){
                        this.model.classifica(classificacao.getKey(), valor);
                        this.viewer.showMessage("O serviço " + classificacao.getKey() + " foi classificado com " + valor);
                    }else
                        this.viewer.showMessage("Valor inválido de classificacão!");
                }catch(EntidadeNaoExistenteException f){
                    this.viewer.showMessage(f.getMessage());
                }catch(NumberFormatException x){
                    this.viewer.showMessage("Valores inválidos");
                }
                break;
            case "5":
                this.utilizadoresComMaisEncomendas();
                break;
            case "6":
                this.transportadorasComMaisKm();
                break;
            case "7":
                try{
                    List<RegistoEntregas> re = this.model.getRegistosUtilizador(codEntity);
                    List<String> auxRe = new ArrayList<>();
                    for(RegistoEntregas temp : re)
                        auxRe.add(temp.info());
                    this.viewer.showList(auxRe,1);
                }catch(EntidadeNaoExistenteException f){
                    this.viewer.showMessage(f.getMessage());
                }
                break;
            case "8":
                String op = this.viewer.menuPerfil();
                atualizaEntidade(codEntity,op, tipo);
                break;
            case "0":
                this.viewer.showMessage("A sair...");
                break;
        }
    }
    /**
     * Método que executa todas as opçoes que um Transportadora do sistema tem acesso
     * @param String [] opcao, String codEntity
     */
    public void opcoesTransportadora(String [] opcao, String codEntity, String tipo){
        Transportadora t = null;
        switch(opcao[0]){
            case "1":
                try{
                    List<Encomenda> encomendasPendentes = this.model.listaDeEncomendasPendentesTransporte(codEntity);
                    List<String> aux = new ArrayList<>();
                    for(Encomenda r : encomendasPendentes)
                        aux.add(r.infoEncomenda());
                    if(aux.size() > 0){
                        this.viewer.showList(aux,3);
                        SimpleEntry<String,String> escolha = this.viewer.escolherEncomendaPendenteTransporte(encomendasPendentes);
                        if(escolha.getKey().equals("2")){
                            String cod = this.model.transportadoraRejeitaEncomenda(codEntity,escolha.getValue());
                            this.viewer.showMessage("A encomenda " + escolha.getValue() + " foi rejeitada. O novo transportador é " + cod + ".");
                        }
                        else if(escolha.getKey().equals("1")){
                            this.model.transportadoraAceitaEncomenda(codEntity,escolha.getValue());
                            this.viewer.showMessage("A encomenda " + escolha.getValue() + " foi aceite.");
                        }
                    }else
                        this.viewer.showMessage("Não há encomendas pendentes.");
                }catch(EntidadeNaoExistenteException e){
                    this.viewer.showMessage(e.getMessage());
                }catch(TransporteNaoExistenteException f){
                    this.viewer.showMessage(f.getMessage());
                }catch(PedidoNaoExistenteException pe){
                    this.viewer.showMessage(pe.getMessage());
                }
                break;
            case "2":
                try{
                    List<Encomenda> ret = this.model.listaDeEncomendasPorEntregar(codEntity);
                    List<String> list = new ArrayList<>();
                    for(Encomenda r : ret)
                        list.add(r.infoEncomenda());
                    this.viewer.showList(list,1);
                }catch(EntidadeNaoExistenteException e){
                    this.viewer.showMessage(e.getMessage());
                }
                break;
            case "3":
                try{
                    List<Encomenda> encPorEntregar = this.model.listaDeEncomendasPorEntregar(codEntity);
                    List<String> list2 = encPorEntregar.stream().map(Encomenda::codigos).collect(Collectors.toList());
                    this.viewer.showList(list2,1);
                    String codigo = this.viewer.escolherEncomenda(encPorEntregar);
                    if(!codigo.equals("")){
                        this.model.entregaEncomenda(codigo);
                        this.viewer.showMessage("A encomenda " + codigo + " foi entregue.");
                    }
                }catch(EntidadeNaoExistenteException e){
                    this.viewer.showMessage(e.getMessage());
                }catch(PedidoNaoExistenteException pe){
                    this.viewer.showMessage(pe.getMessage());
                }
                break;
            case "4":
                this.utilizadoresComMaisEncomendas();
                break;
            case "5":
                this.transportadorasComMaisKm();
                break;
            case "6":
                try{
                    List<RegistosTransporte> regs = this.model.registosTransporte(codEntity);
                    List<String> aux2 = new ArrayList<>();
                    for(RegistosTransporte r : regs)
                        aux2.add(r.info());
                    this.viewer.showList(aux2,1);
                }catch(EntidadeNaoExistenteException e){
                    this.viewer.showMessage(e.getMessage());
                }
                break;
            case "7":
                SimpleEntry<LocalDateTime,LocalDateTime> intervalo = this.viewer.getIntervalo();
                try{
                    double ganho = this.model.totalFaturadoPorEmpresa(codEntity,intervalo.getKey(),intervalo.getValue());
                    this.viewer.showMessage("Os ganhos da empresa " + codEntity + " no intervalo de tempo pretendido foram: " + ganho);
                }
                catch(EntidadeNaoExistenteException e){
                    this.viewer.showMessage("A entidade não existe.");
                }
                break;
            case "8":
                registosIntervaloTempo(codEntity);
                break;
            case "9":
                String op = this.viewer.menuPerfil();
                atualizaEntidade(codEntity,op, tipo);
                break;
            case "0":
                this.viewer.showMessage("A sair...");
                break;
        }
    }
	/**
     * Método que executa todas as opçoes que um Voluntario do sistema tem acesso
     * @param String [] opcao, String codEntity
     */
    public void opcoesVoluntario(String [] opcao, String codEntity, String tipo){
        Voluntario v = null;
        switch(opcao[0]){
            case "1":
                try{
                    List<Encomenda> encPorEntregar = this.model.listaDeEncomendasPorEntregar(codEntity);

                    List<String> list2 = encPorEntregar.stream().map(Encomenda::codigos).collect(Collectors.toList());
                    if(!list2.isEmpty()){
                        this.viewer.showList(list2,1);
                        String codigo = this.viewer.escolherEncomenda(encPorEntregar);
                        if(!codigo.equals("0")){
                            this.model.entregaEncomenda(codigo);
                            this.viewer.showMessage("A encomenda " + codigo + " foi entregue.");
                        }
                    }else
                        this.viewer.showMessage("Não existem encomendas prontas para entregar.");
                }catch(EntidadeNaoExistenteException e){
                    this.viewer.showMessage(e.getMessage());
                }catch(PedidoNaoExistenteException pe){
                    this.viewer.showMessage(pe.getMessage());
                }
                break;
            case "2":
                try{
                    List<Encomenda> ret = this.model.listaDeEncomendasPendentesTransporte(codEntity);
                    List<String> list = new ArrayList<>();
                    for(Encomenda r : ret)
                        list.add(r.infoEncomenda());
                    this.viewer.showList(list,3);
                    SimpleEntry<String,String> escolha = this.viewer.escolherEncomendaPendenteTransporte(ret);
                    if(escolha.getKey().equals("2")){
                        String cod = this.model.voluntarioRejeitaEncomenda(codEntity,escolha.getValue());
                        this.viewer.showMessage("A encomenda "+ escolha.getValue() + " foi rejeitada. O novo transportador é " + cod + ".");
                    }
                    else if(escolha.getKey().equals("1")){
                        this.model.voluntarioAceitaEncomenda(escolha.getValue());
                        this.viewer.showMessage("A encomenda "+ escolha.getValue() + " foi aceite.");
                    }
                }catch(EntidadeNaoExistenteException e){
                    this.viewer.showMessage(e.getMessage());
                }catch(TransporteNaoExistenteException x){
                    this.viewer.showMessage(x.getMessage());
                }catch(PedidoNaoExistenteException pe){
                    this.viewer.showMessage(pe.getMessage());
                }
                break;
            case "3":
                this.utilizadoresComMaisEncomendas();
                break;
            case "4":
                this.transportadorasComMaisKm();
                break;  
            case "5":
                try{
                    List<RegistosTransporte> regs = this.model.registosTransporte(codEntity);
                    List<String> aux2 = new ArrayList<>();
                    for(RegistosTransporte r : regs)
                        aux2.add(r.info());
                    this.viewer.showList(aux2,1);
                }catch(EntidadeNaoExistenteException e){
                    this.viewer.showMessage(e.getMessage());
                }
                break;
            case "6":
                registosIntervaloTempo(codEntity);
                break;
            case "7":
                String op = this.viewer.menuPerfil();
                atualizaEntidade(codEntity,op, tipo);
                break;
            case "0":
                this.viewer.showMessage("A sair...");
                break;
        }
    }
    /**
     * Método que executa todas as opçoes que uma Loja do sistema tem acesso
     * @param String [] opcao, String codEntity
     */
	public void opcoesLoja(String [] opcao, String codEntity, String tipo){
        Loja loja = null;
		switch(opcao[0]){
			case "1":
				List<Encomenda> encPorSinalizar = new ArrayList<>();
				try{
					encPorSinalizar = this.model.listaDeEncomendas(codEntity);
				}catch(EntidadeNaoExistenteException e){
					this.viewer.showMessage(e.getMessage());
				}
				if(encPorSinalizar.size() > 0){
					List<String> aux = new ArrayList<>();
					for(Encomenda r : encPorSinalizar)
					aux.add(r.codigos());
					this.viewer.showList(aux,3);
					String codigo = this.viewer.escolherEncomenda(encPorSinalizar);
					if(!codigo.equals("0")){
						try{
							this.model.processarProxEncomenda(codEntity, codigo);
							this.viewer.showMessage("Encomenda de código " + codigo + " foi processada");    
						}catch(EntidadeNaoExistenteException e){
							this.viewer.showMessage(e.getMessage());
						}
					}
				}
				break;
			case "2":
				try{
					List<Encomenda> fila = this.model.listaDeEncomendas(codEntity);
					List<String> aux2 = new ArrayList<>();
					for(Encomenda r : fila)
						aux2.add(r.toString());
					this.viewer.showList(aux2,1);
				}catch(EntidadeNaoExistenteException e){
					this.viewer.showMessage(e.getMessage());
				}
				break;
			case "3":
				this.utilizadoresComMaisEncomendas();
				break;
			case "4":
				this.transportadorasComMaisKm();
				break;
			case "5":
				Produto p = this.viewer.fazProduto();
				if(p != null){
					try{
						this.model.adicionaProdutoLoja(codEntity, p);
					}
					catch(EntidadeNaoExistenteException e){
						this.viewer.showMessage(e.getMessage());
					}
				}
				break;
            case "6":
                String op = this.viewer.menuPerfil();
                atualizaEntidade(codEntity,op, tipo);
                break;
			case "0":
				this.viewer.showMessage("A sair...");
				break;
		}
	}
    /**
     * Metodo que devolve os RegistosTransportes num intervalo de tempo de uma dada Entidade.
     * @param String codEntity
     */  
    private void registosIntervaloTempo(String codEntity){
        SimpleEntry<LocalDateTime,LocalDateTime> intervalo = this.viewer.getIntervalo();
        try{
            List <RegistosTransporte> regs = this.model.registosIntervalo(codEntity, intervalo.getKey(), intervalo.getValue());
            List <String> vals = new ArrayList<>();
            for(RegistosTransporte r : regs)
                vals.add(r.info());
            this.viewer.showList(vals,3);
        }
        catch(EntidadeNaoExistenteException e){
            this.viewer.showMessage(e.getMessage());
        }
    }

	/**
	 * Método que cria um Map com todos os produtos de uma loja
	 * @param String cod
	 * @return Map<String,Produto>
	 */
	public Map<String,Produto> getProdutos(String cod){
		Map<String,Produto> ret = new HashMap<>();
		try{
			ret = this.model.getProdutosLoja(cod);
		}catch(EntidadeNaoExistenteException e){
			this.viewer.showMessage(e.getMessage());
		}
		return ret;
	}
	/**
     * Método que grava o estado do sistema num ficheiro
     * @param String filePath
     */
	public void gravarEstado(String filePath) throws FileNotFoundException, IOException{
		FileOutputStream f = new FileOutputStream(filePath);
		ObjectOutputStream o = new ObjectOutputStream(f); 
		o.writeObject(this);
		o.flush();
		o.close();
	}
	 /**
	 * Método que carrega o estado do sistema do ficheiro
	 * @param String filePath
	 * @return Sistema
	 */
	public void carregarEstado(String filePath) throws FileNotFoundException,IOException,ClassNotFoundException{
		FileInputStream f = new FileInputStream(filePath);
		ObjectInputStream o = new ObjectInputStream(f);
		IController g = (IController) o.readObject();
        this.viewer = g.getViewer();
        this.model = g.getModel();
		o.close();
		
	}
    /**
     * Metodo que verifica se existe uma loja atraves do seu codigo
     * @param String l
     * @return boolean resultante da verificaçao
     */
    public boolean existeLoja(String l){
        return this.model.existeCodigo(l);
    }
    /**
     * Metodo que imprime com a ajuda do Navegador os Utilizadores que mais usaram o sistema
     */
    private void utilizadoresComMaisEncomendas(){
        StringBuilder sb = new StringBuilder();
        List<SimpleEntry<String,Integer>> users = this.model.top10Utilizadores();
        if(users.size()> 0){
            List<String> auxUsers = new ArrayList<>();
            for(SimpleEntry<String,Integer> e1 :users){
                sb = new StringBuilder();
                sb.append("Utilizador: ").append(e1.getKey()).append("\n").append("Número de encomendas: ").append(e1.getValue()).append("\n");
                auxUsers.add(sb.toString());
            }
                this.viewer.showList(auxUsers,10);
            }else
                this.viewer.showMessage("Nenhum utilizador fez encomendas\n");
    }
    /**
     * Metodo que imprime com a ajuda do Navegador as transportadoras/voluntarios com mais quilometros percorridos
     */
    private void transportadorasComMaisKm(){
        List<SimpleEntry<String,Double>> emp = this.model.top10EmpresasKmPercorridos();
        if(emp.size() > 0){
            StringBuilder sb2 = new StringBuilder();
            List<String> auxTransp = new ArrayList<>();
            for(SimpleEntry<String,Double> e2 : emp){
                sb2 = new StringBuilder();
                sb2.append("Empresa: ").append(e2.getKey()).append("\t").append("Kilometros: ").append(e2.getValue()).append("\n");
                auxTransp.add(sb2.toString());
            }
                this.viewer.showList(auxTransp,10);
            }else
                this.viewer.showMessage("Nenhuma empresa fez entregas\n");
    }

    public void getPedidosSistema(){
        List<String> ret = this.model.getPedidos().getPedidos().values().stream().map(Pedido::simpleToString).collect(Collectors.toList());
        this.viewer.showList(ret, 5);
    }
}
