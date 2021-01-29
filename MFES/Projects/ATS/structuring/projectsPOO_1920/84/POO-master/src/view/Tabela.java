package view;

import java.util.List;

public class Tabela<T> implements ITable{ //T pode ser qualquer coisa

    private final List<String> linha;
    private final List<String> coluna;
    private final List<List<T>> iT;
    private final StringBuilder build;

    public Tabela(List<String> linha,List<String> coluna, List<List<T>> valores) {
        this.linha = linha;
        this.coluna = coluna;
        this.iT = valores;
        this.build = new StringBuilder();
    }


    public String toString(){
        build.setLength(0);
        int li = this.linha.size();
        int col = this.coluna.size();
        StringP espaco = new StringP(" ");

        //Basicamente estamos a criar o tamanho ajustado de da tabela para
        // que cada titulo(cada v.i ou detalhe de por exemplo uma encomenda
        // tenha espaco para ser representada

        int[] sizeCol = new int[col+1];
        int titulosTamanho = 0;
        for(String s: this.linha){
            if(titulosTamanho < s.length()) titulosTamanho = s.length(); //ver o maior titulo que possui
        }
        sizeCol[0] = titulosTamanho +2;

        int l = 0;
        int c =0;
        for(c = 0; c<col ; c++){
            sizeCol[c+1] = this.coluna.get(c).length()+2;
            for(l=0; l<li ; l++){
                if(sizeCol[c + 1] < this.iT.get(l).get(c).toString().length() + 2)
                    sizeCol[c+1] = this.iT.get(l).get(c).toString().length() +2;
            }
        }

        //print de uma col
        this.separadoDeLinha(sizeCol);
        build.append("|");
        build.append(espaco.repeat(sizeCol[0])); // numero de espacos
        for(c = 0; c<col; c++){
            build.append("| ").append(this.coluna.get(c));
            build.append(espaco.repeat(sizeCol[c+1] - this.coluna.get(c).length()-1)); //stringbuilder responsavel
        }
        build.append("\n");
        this.separadoDeLinha(sizeCol);

        //print das cenas
        for(l=0; l<li;l++) {
            build.append("| ").append(this.linha.get(l));
            build.append(espaco.repeat(sizeCol[0] - this.linha.get(l).length() - 1));
            for (c = 0; c < col; c++) {
                build.append("| ").append(this.iT.get(l).get(c).toString());
                build.append(espaco.repeat(sizeCol[c + 1] - this.iT.get(l).get(c).toString().length() - 1));
            }

            build.append("|\n");
            this.separadoDeLinha(sizeCol);
        }

        return build.toString();
    }


    private void separadoDeLinha(int[] sizeCol) { //print
        StringP sif = new StringP("-");
        int c;
        for (c = 0; c <= sizeCol.length - 1; c++)
            this.build.append("+").append(sif.repeat(sizeCol[c])).toString();
        this.build.append("+\n");
    }


}
