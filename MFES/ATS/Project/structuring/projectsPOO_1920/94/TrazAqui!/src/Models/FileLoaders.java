package Models;

import View.ViewError;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.security.InvalidParameterException;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class FileLoaders implements Serializable {

    public FileLoaders() {
    }

    public Sistema carregaEstado(String nomeFicheiro) throws IOException {
        FileInputStream inf = new FileInputStream(nomeFicheiro);
        ObjectInputStream io = new ObjectInputStream(inf);
        Sistema res=null;
        try {
            res = (Sistema) io.readObject();
        }catch (Exception e){throw new IOException("Erro a carregar ficheiro");}
        io.close();
        return res;
    }

    public void guardaEstado(String nomeFicheiro, Sistema fds) throws IOException {
        FileOutputStream of = new FileOutputStream(nomeFicheiro);
        ObjectOutputStream oo = new ObjectOutputStream(of);

        oo.writeObject(fds);
        oo.flush();
        oo.close();

    }

    public void loadLogs(Sistema s) {
        List<String> linhas = lerFicheiro("./docs/logs_20200416.txt");
        String[] linhaPartida;
        List<String> encs = new ArrayList<>();
        List<String> aceites = new ArrayList<>();
        for (String linha : linhas) {
            linhaPartida = linha.split(":", 2);
            switch (linhaPartida[0]) {
                case "Utilizador" -> parseUtilizador(linhaPartida[1], s);
                case "Loja" -> parseLoja(linhaPartida[1], s);
                case "Transportadora" -> parseEmpresa(linhaPartida[1], s);
                case "Voluntario" -> parseVoluntario(linhaPartida[1], s);
                case "Encomenda" -> encs.add(linhaPartida[1]);
                case "Aceite" -> aceites.add(linhaPartida[1]);
            }
        }
        for (String e : encs) {
            parseEncomenda(e, aceites, s);
        }
    }

    public void parseEncomenda(String input, List<String> aceites, Sistema s) {

        String[] campos = input.split(",");
        String codEncomenda = campos[0];

        boolean aceite = false;
        for (String a : aceites) {
            if (a.equals(codEncomenda)) {
                aceite = true;
                break;
            }
        }
        if (aceite) {
            int tamanho = campos.length;
            String codUtilizador = campos[1];
            String codLoja = campos[2];
            double peso = Double.parseDouble(campos[3]);

            Encomenda res = new Encomenda(codLoja, codUtilizador, codEncomenda, peso);

            for (int i = 4; i < tamanho; i += 4) {
                try {
                    res.add_linha(new Produto(campos[i], campos[i + 1], Double.parseDouble(campos[i + 3]), Double.parseDouble(campos[i + 2]), -1));
                } catch (NullPointerException e) {
                    e.printStackTrace();
                }
            }
            try{
            s.inserirPedidoEncomenda(res);
            }catch (InvalidParameterException ignored){
            }
        }
    }


    public void parseUtilizador(String input, Sistema s) {
        String mail = "@trazaqui.pt";
        String[] campos = input.split(",");
        String codUtilizador = campos[0];
        String nome = campos[1];

        try {
            double gpsx = Double.parseDouble(campos[2]);
            double gpsy = Double.parseDouble(campos[3]);

            s.registarUtilizador(codUtilizador, codUtilizador + mail, codUtilizador, nome, gpsx, gpsy);
        } catch (Exception ignored) {
        }
    }

    public void parseVoluntario(String input, Sistema s) {
        String[] campos = input.split(",");
        String codVoluntario = campos[0];
        String nomeVoluntario = campos[1];
        try {
            double gpsx = Double.parseDouble(campos[2]);
            double gpsy = Double.parseDouble(campos[3]);
            double raio = Double.parseDouble(campos[4]);

            s.registarVoluntario(codVoluntario, nomeVoluntario, gpsx, gpsy, raio, gpsx > 50, true);
        } catch (Exception ignored) {
        }
    }


    public void parseEmpresa(String input, Sistema s) {
        String[] campos = input.split(",");
        String codEmpresa = campos[0];
        String nomeEmpresa = campos[1];

        try {
            double gpsx = Double.parseDouble(campos[2]);
            double gpsy = Double.parseDouble(campos[3]);
            double raio = Double.parseDouble(campos[4]);
            String NIF = campos[5];
            double taxa = Double.parseDouble(campos[6]);
            s.registarEmpresa(codEmpresa, nomeEmpresa, gpsx, gpsy, raio, taxa, 1 + (int) (Math.random() * ((10 - 1) + 1)), NIF, gpsx > 50, true);
        } catch (Exception ignored) {
        }
    }


    public void parseLoja(String input, Sistema s) {
        String[] campos = input.split(",");
        String codLoja = campos[0];
        String nomeLoja = campos[1];

        try {
            double gpsx = Double.parseDouble(campos[2]);
            double gpsy = Double.parseDouble(campos[3]);
            s.registarLoja(codLoja, nomeLoja, gpsx, gpsy, Math.random() % 2 == 0, "mercearia");
        } catch (Exception ignored) {
        }
    }

    public List<String> lerFicheiro(String nomeFich) {
        List<String> lines = new ArrayList<>();
        try {
            lines = Files.lines(Paths.get(nomeFich), StandardCharsets.UTF_8).skip(43).collect(Collectors.toList());
        } catch (IOException ignored) {
        }
        return lines;
    }

}
