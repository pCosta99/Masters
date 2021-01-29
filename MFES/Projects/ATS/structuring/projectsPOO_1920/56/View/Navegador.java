package View;

import Model.Catalogos.ICatalogoProds;
import Model.Catalogos.IProduto;
import Model.Encomendas.ILinhaEncomenda;
import Model.Tipos.ILoja;
import Model.Tipos.ITipo;
import Model.Tipos.Loja;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Navegador implements INavegador {
    private int tamPag; //nº de strings por pagina ou seja tamanho da pagina
    private int nTPag; //nº total de páginas
    private int pagina; // nº da página atual
    private int inseridos; //nº de strings já inseridas

    public Navegador(){
        this.tamPag = 20;
        this.nTPag = 0;
        this.pagina = 1;
        this.inseridos = 0;
    }

    public void divide(ICatalogoProds cat, Set<ITipo> lojas, String mensagem, int opcao) {
        List<Loja> aList = new ArrayList<>();
        List<IProduto> prods = new ArrayList<>();
        if(opcao==0){
            for (IProduto prod : cat.getCatProds().values()) {
                prods.add(prod);
            }
        }


        if (opcao == 1) {
            for (ITipo x : lojas){
                if(x instanceof  Loja) aList.add((Loja) x);
            }
        }
        System.out.print("\033[H\033[2J"); //limpa ecra
        System.out.flush(); //limpa ecra

        int i;
        if(opcao==0) {
            if((prods.size()%20)!=0)this.nTPag = (prods.size() / 20) +1;
            else this.nTPag = (prods.size() / 20);

            if((this.pagina <= this.nTPag) && (this.inseridos <= prods.size())){
                System.out.println(mensagem);
                System.out.println("«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»");
                System.out.print("Código    Nome                 Preço                  ");
                System.out.print("      Código    Nome                     Preço\n");
                System.out.println("«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»");
                for(i = this.inseridos; (i< (this.tamPag + this.inseridos)) && (i <prods.size()); i++ ){
                    System.out.printf("%-10s" ,prods.get(i).getCodProduto());
                    System.out.printf("%-20s" , prods.get(i).getNome());
                    System.out.printf("%-30s", prods.get(i).getPreco());
                    int j=i+10;
                    System.out.printf("%-10s",prods.get(j).getCodProduto());
                    System.out.printf("%-25s",prods.get(j).getNome());
                    System.out.printf("%-20s",prods.get(j).getPreco());
                    System.out.println();
                    if(i==(this.inseridos+9)) break;
                }
                if(i>=cat.totalProds()) System.out.println("Fim dos resultados.");
                System.out.printf("\nPágina <%d/%d> \n\n",this.pagina ,this.nTPag);
            }
        }
        if (opcao==1){
            if((aList.size()%20)!=0)this.nTPag = (prods.size() / 20) +1;
            else this.nTPag = (aList.size() / 20);
            if((this.pagina <= this.nTPag) && (this.inseridos <= aList.size())){
                System.out.println(mensagem);
                System.out.println("«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»");
                System.out.print("Código    Nome           ");
                System.out.print("      Código    Nome                     \n");
                System.out.println("«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»«»");
                for(i = this.inseridos; (i< (this.tamPag + this.inseridos)) && (i <aList.size()); i++ ){
                    System.out.printf("%-10s" ,aList.get(i).getId());
                    System.out.printf("%-20s" , aList.get(i).getNome());
                    int j=i+10;
                    if(j<=aList.size()){

                    System.out.printf("%-10s",aList.get(j).getId());
                    System.out.printf("%-25s",aList.get(j).getNome());}
                    else {
                        System.out.printf("sem mais resultados");
                    }
                    System.out.println();
                    if(i==(this.inseridos+9)) break;
          /*  if((this.pagina <= this.nTPag) && (this.inseridos <= lojas.size())){
                System.out.println(mensagem);
                for(i = this.inseridos; (i< (this.tamPag + this.inseridos)) && (i <lojas.size()); i++ ){
                    System.out.print("\n********LOJA*********\n");
                    System.out.println(aList.get(i).toString());
                    if(i==(this.inseridos+1)) break;
            */    }
                if(i>=aList.size()) System.out.println("Fim dos resultados.");
                System.out.printf("\nPágina <%d/%d> \n\n",this.pagina ,this.nTPag);

            }
        }
    }

    public void proxima(ICatalogoProds cat, Set<ITipo> lojas, String mensagem, int opcao){
        this.pagina += 1;
        this.inseridos += 20;
        divide(cat,lojas,mensagem,opcao);
    }

    public void anterior(ICatalogoProds cat, Set<ITipo> lojas, String mensagem, int opcao){
        this.pagina -= 1;
        this.inseridos -= 20;
        if((this.pagina >= 0) && (this.inseridos >=0)){
            divide(cat,lojas,mensagem,opcao);
        }
        else {
            System.out.print("\nPágina Inválida\n\n");
        }
    }

    public void total(ICatalogoProds cat,Set<ITipo> lojas, int opcao){
        if (opcao==0)  System.out.print("Total: " + cat.totalProds() + "\n\n");
        if (opcao==1) System.out.print("Total: "+ lojas.size()+ "\n\n");
    }

    public void escolha(ICatalogoProds cat, Set<ITipo> lojas, String mensagem, int opcao,int num){
        this.pagina = num;
        this.inseridos = num*2;
        divide(cat,lojas,mensagem,opcao);
    }

    public void menu (){
        System.out.println("Próxima Página(P)             Menu(M)                     Página Anterior(A)");
        System.out.println("Escolha o nº da Página(N)     Efetuar Encomenda (E)           Total(T)      ");
    }
}
