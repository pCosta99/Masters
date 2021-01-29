import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
import java.util.List;
import java.util.ArrayList;

public class ReadLogs
{
	public void addEncomenda(StateManager state, String[] parts)
	{
		String encoCod = parts[0].substring(10,parts[0].length());
	
		List<LinhaEncomenda> listaProdutos = new ArrayList<>();

		for(int i = 4; i < parts.length ; i += 4)
		{
			LinhaEncomenda linha = new LinhaEncomenda(parts[i],parts[i+1],Double.valueOf(parts[i+2]),Double.valueOf(parts[i+3]));
			listaProdutos.add(linha);
		}

		Encomenda encomenda = new Encomenda(encoCod, parts[2], parts[1], Double.valueOf(parts[3]), listaProdutos);
		Utilizador user = (Utilizador) state.getUser(parts[1]+"@email.pt");
		Loja loja = (Loja) state.getUser(parts[2]+"@email.pt");
		Voluntario closerVol = user.retornacloseVol(user.getLoc(),loja.getLoc(),state.getUserList());
		EmpresaV closerTrans = user.retornacloseEmp(user.getLoc(),loja.getLoc(),state.getUserList());
		
		if(closerVol != null)
		{
			state.getUser(closerVol.getEmail()).addEncomenda(encomenda);
		}
        else if(closerTrans != null)
			state.getUser(closerTrans.getEmail()).addEncomenda(encomenda);
        

		state.addEcoAceite(encoCod,false);
		
		for(int i = 1; i < 3; i++)
		{	
			Account aux = state.getUserByCode(parts[i]);

			if(aux != null) 
				aux.addEncomenda(encomenda);
		}
	}

    public void read(StateManager state)
    {
        try
        {
            File file = new File("logs.txt");
            Scanner myReader = new Scanner(file);
            
            while (myReader.hasNextLine()) 
            {
                String data = myReader.nextLine();

                String[] parts = data.split(",");

                if(parts[0].charAt(0) == 'U')
                {
                    String userCod = parts[0].substring(11,parts[0].length());
                    state.addUser(new Utilizador(parts[1],(userCod+"@email.pt"),userCod,new ArrayList<Encomenda>(),
                    userCod, new Location(Double.valueOf(parts[2]),Double.valueOf(parts[3]))));
                }

                if(parts[0].charAt(0) == 'V')
                {
                    String volCod = parts[0].substring(11,parts[0].length());
                    state.addUser(new Voluntario(new ArrayList<Encomenda>(), (volCod+"@email.pt"), volCod, volCod,
                    parts[1], new Location(Double.valueOf(parts[2]),Double.valueOf(parts[3])), Double.valueOf(parts[4]), new Encomenda()));
                }

                if(parts[0].charAt(0) == 'T')
                {
                	Location l = new Location(Double.valueOf(parts[2]),Double.valueOf(parts[3]));
                    String transCod = parts[0].substring(15,parts[0].length());
                    state.addUser(new EmpresaV(new ArrayList<Encomenda>(), (transCod+"@email.pt"), transCod, transCod,
                    parts[1], l, Double.valueOf(parts[5]), parts[4], Double.valueOf(parts[6])));

                    //System.out.println(state.getUser(transCod+"@email.pt"));
                }

                if(parts[0].charAt(0) == 'L')
                {
                    String lojaCod = parts[0].substring(5,parts[0].length());
                    state.addUser(new Loja((lojaCod+"@email.pt"), lojaCod, new ArrayList<Encomenda>(), parts[1],
                    lojaCod, new Location(Double.valueOf(parts[2]),Double.valueOf(parts[3]))));
                }

                if(parts[0].charAt(0) == 'E')
                  	addEncomenda(state,parts);
       
                if(parts[0].charAt(0) == 'A')
                {
                  	state.addEcoAceite(parts[0].substring(7,parts[0].length()),true);	
                }

            }
            myReader.close();
        } 
        catch (FileNotFoundException e) 
        {
            System.out.println("Ficheiro logs.txt inexistente");
            e.printStackTrace();
        }
    }
}