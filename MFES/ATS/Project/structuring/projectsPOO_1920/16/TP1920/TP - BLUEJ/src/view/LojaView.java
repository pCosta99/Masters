package src.view;

import src.exceptions.EncomendaInvalidaException;
import src.exceptions.LojaInexistenteException;
import src.exceptions.ValorInvalidoException;
import src.model.Encomenda;
import src.model.Loja;
import src.controller.LojaController;

import java.util.Set;
import java.util.Scanner;




public class LojaView{
	private LojaController controller;
	private Scanner sc;

	public LojaView(LojaController controller,Scanner sc){
		this.controller = controller;
		this.sc = sc;
	}

	public void run(){
		int x = 1;
		while(x != 0){
			System.out.println("Escolha uma opção:");
			System.out.println("0 - Sair");
			System.out.println("1 - Registar Produto");
			System.out.println("2 - Registar Medicamento");
			System.out.println("3 - Ver Encomendas a serem preparadas");
			System.out.println("4 - Sinalizar Encomenda pronta");
			System.out.println("5 - Sinalizar Tempo médio de Atendimento");
            System.out.println("6 - Verificar Perfil");

            try{
    			x = sc.nextInt();
    			sc.nextLine();

    			switch(x){

    				case (1): 
                        this.registarProdutoSc(false,sc);
                        break;

                   case (2): 
                        this.registarProdutoSc(true,sc);
                        break;

                   case (3): 
                        this.showEncomendasAceites();
                        break;

                   case (4): 
                        this.sinalizaEntregaProntaSc();
                        break;

                   case (5): 
                        this.setTempoAtendimentoSc();
                        break;

                    case(6):
                        this.showPerfil(this.controller.getPerfil());
                        break;

                   case (0):
                       System.out.println("Até breve!");
                        break;

                    default:
                        System.out.println("Insira uma opção válida");
                }
            }

            catch(RuntimeException e){
                System.out.println("Não foi possível Realizar o solicitado!");
                System.out.println(e.getMessage());
                sc.nextLine();
            }
        }
   }


    public void registarProdutoSc(Boolean isMedicamento,Scanner sc) throws RuntimeException{
        String nome;
        double custo;
        double peso;
        System.out.println("Insira o nome do produto:");
        nome = sc.nextLine();
        System.out.println("Insira o preço unitário do produto:");
        custo = sc.nextDouble();
        System.out.println("Insira o peso unitário do produto:");
        peso = sc.nextDouble();

        try {
            this.controller.registaProduto(nome, custo, peso, isMedicamento);
        }

        catch (ValorInvalidoException v){
            System.out.println("Valor Inválido!");
        }
    }

    

    public void getEncomendasAceitesVw(Set<Encomenda> printables){
        int r = 0;
        for (Encomenda e : printables){
            System.out.println(r + "-" + e);
            r++;
        }

        System.out.println("");
    }

    


    public void showEncomendasAceites(){
        this.getEncomendasAceitesVw(this.controller.getEncomendasAceites());
    }

    


    public void sinalizaEntregaProntaSc(){
        Set<Encomenda> encs = this.controller.getEncomendasAceites();
        this.getEncomendasAceitesVw(encs);
        String sel;
        System.out.println("Selecione o código da Encomenda Pronta:");
        sel = sc.nextLine();
        try{
            this.controller.sinalizaEntregaPronta(sel);
        }

        catch(EncomendaInvalidaException e){
            System.out.println("Não foi possível realizar o solicitado;");
            System.out.println(e.getMessage());
        }

    }

    public void setTempoAtendimentoSc() throws RuntimeException{
        try{
            Double atendimento;
            System.out.println("Insira o valor do tempo de atendimento médio:");
            atendimento = sc.nextDouble();
            this.controller.setTempoAtendimento(atendimento);
        }

        catch(ValorInvalidoException e){
            System.out.println("Não foi possível realizar o solicitado;");
            System.out.println(e.getMessage());
        }


    }

    public void showPerfil(Loja atual){
        System.out.println("Código de user:");
        System.out.println(atual.getUsername());
        System.out.println("Nome:");
        System.out.println(atual.getNome());
        System.out.println("Email:");
        System.out.println(atual.getEmail());
        System.out.println("Localização:");
        System.out.println(atual.getLocalizacao());
    }
}
