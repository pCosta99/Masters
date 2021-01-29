package Models;

import java.util.ArrayList;
import java.util.List;

public class ListaProdutos {
    private final ArrayList<Produto> restaurante;
    private final ArrayList<Produto> mercearia;
    private final ArrayList<Produto> informatica;
    private final ArrayList<Produto> vestuario;


    public ListaProdutos() {
        restaurante = new ArrayList<>();
        mercearia = new ArrayList<>();
        informatica = new ArrayList<>();
        vestuario = new ArrayList<>();

        add_produtos_restaurante();
        add_produtos_mercearia();
        add_produtos_mercearia();
        add_produtos_vestuario();


    }

    public static List<Produto> getArrayList(List<Produto> x) {
        ArrayList<Produto> res = new ArrayList<>();
        for (Produto e : x) {
            res.add(e.clone());
        }
        return res;
    }

    public void add_produtos_restaurante() {
        Produto produto = new Produto("p1", "Bacalhau com Natas", 10, 1, 0.6631);
        restaurante.add(produto.clone());

        produto = new Produto("p2", "Francesinha Especial", 8, 1, 0.6);
        restaurante.add(produto.clone());

        produto = new Produto("p3", "Cabrito assado no Forno", 20, 1, 1.2);
        restaurante.add(produto.clone());

        produto = new Produto("p4", "Polvo", 25, 1, 0.9);
        restaurante.add(produto.clone());

        produto = new Produto("p5", "Barriga na grelha", 9, 1, 0.2);
        restaurante.add(produto.clone());

        produto = new Produto("p6", "Hamburguer no pão", 5, 1, 0.4);
        restaurante.add(produto.clone());

        produto = new Produto("p7", "Garrafa de água 1L", 1, 1, 1.0);
        restaurante.add(produto.clone());

        produto = new Produto("p8", "Vinho Tinto da Casa 1,5L", 7, 1, 1.5);
        restaurante.add(produto.clone());

    }

    public void add_produtos_mercearia() {
        Produto produto = new Produto("p1", "Pão (6 Unidades)", 1, 1, 0.102);
        mercearia.add(produto.clone());

        produto = new Produto("p2", "Leite Meio-Gordo 1L", 0.5, 1, 1.0);
        mercearia.add(produto.clone());

        produto = new Produto("p3", "Ovos (6 unidades)", 2, 1, 0.3);
        mercearia.add(produto.clone());

        produto = new Produto("p4", "Cereais Chocapic 1kg", 3, 1, 1.0);
        mercearia.add(produto.clone());

        produto = new Produto("p5", "Fiambre de Peru 300g", 2, 1, 0.3);
        mercearia.add(produto.clone());

        produto = new Produto("p6", "Queijo Fatiado 300g", 3, 1, 0.3);
        mercearia.add(produto.clone());

        produto = new Produto("p7", "Garrafa de água 1L", 0.5, 1, 1.0);
        mercearia.add(produto.clone());

        produto = new Produto("p8", "Bolachas Maria 500g", 2, 1, 0.5);
        mercearia.add(produto.clone());

        produto = new Produto("p9", "Detergente Roupa", 4, 1, 3.4);
        mercearia.add(produto.clone());

        produto = new Produto("p10", "Sabão Aloe Vera", 2, 1, 0.06);
        mercearia.add(produto.clone());

        produto = new Produto("p11", "Champô Linic", 2, 1, 0.4);
        mercearia.add(produto.clone());

        produto = new Produto("p12", "Bananas (6 unidades)", 1.82, 1, 0.2);
        mercearia.add(produto.clone());

        produto = new Produto("p13", "Gelados Magnum (4 unidades)", 3, 1, 0.5);
        mercearia.add(produto.clone());

        produto = new Produto("p14", "1 frango", 2.56, 1, 2.45);
        mercearia.add(produto.clone());

        produto = new Produto("p15", "Vassoura", 4.5, 1, 0.8);
        mercearia.add(produto.clone());

    }

    public void add_produtos_informatica() {
        Produto produto = new Produto("p1", "Televisao Samsung 36Inch", 899.99, 1, 4.0);
        informatica.add(produto.clone());

        produto = new Produto("p2", "Portatil Lenovo Y540", 854.99, 1, 2.0);
        informatica.add(produto.clone());

        produto = new Produto("p3", "Portatil Apple Macbook Air 2020 13''", 999.99, 1, 0.001);
        informatica.add(produto.clone());

        produto = new Produto("p4", "Comando TV Universal", 15, 1, 0.032);
        informatica.add(produto.clone());

        produto = new Produto("p5", "Pilhas AAA", 2.20, 1, 0.1);
        informatica.add(produto.clone());

        produto = new Produto("p6", "Telemovel Samsung A70", 350, 1, 0.183);
        informatica.add(produto.clone());

        produto = new Produto("p7", "Telemovel IPhone 10 XS", 1050.99, 1, 0.031);
        informatica.add(produto.clone());

        produto = new Produto("p8", "Pack teclado e rato USB - Preto", 25.99, 1, 0.7);
        informatica.add(produto.clone());

    }

    public void add_produtos_vestuario() {
        Produto produto = new Produto("p1", "T-Shirt Homem - XL Preto", 15.99, 1, 0.08);
        vestuario.add(produto.clone());

        produto = new Produto("p2", "Casaco Pele Mulher - M - Castanho", 50.99, 1, 0.10);
        vestuario.add(produto.clone());

        produto = new Produto("p3", "Gorro GUCCI", 10000, 1, 0.06);
        vestuario.add(produto.clone());

        produto = new Produto("p4", "Calcas Ganga Crianca ", 20, 1, 0.2);
        vestuario.add(produto.clone());

        produto = new Produto("p5", "Sapato Homem - Tam. 42", 69.99, 1, 0.4);
        vestuario.add(produto.clone());

        produto = new Produto("p6", "Top Mulher - Beje", 10.99, 1, 0.1);
        vestuario.add(produto.clone());

        produto = new Produto("p7", "Boxers GUCCI", 3660, 1, 0.03);
        vestuario.add(produto.clone());

        produto = new Produto("p8", "Meias da Avó (10 pares)", 12, 1, 0.4);
        vestuario.add(produto.clone());

    }

    public List<Produto> generateList(String tipo) {
        List<Produto> res = null;

        switch (tipo) {
            case "informatica" -> res = ListaProdutos.getArrayList(this.informatica);
            case "restaurante" -> res = ListaProdutos.getArrayList(this.restaurante);
            case "vestuario" -> res = ListaProdutos.getArrayList(this.vestuario);
            case "mercearia" -> res = ListaProdutos.getArrayList(this.mercearia);
            default -> res = ListaProdutos.getArrayList(this.mercearia); // poderia ser melhorado
        }

        return res;
    }


}
