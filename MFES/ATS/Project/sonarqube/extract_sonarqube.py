from sonarqube import SonarQubeClient
import sys
import csv

url = 'http://localhost:9000'
username = "admin"
password = "admin"
filename = sys.argv[1]
delete = sys.argv[2]
# connect to sonarqube
sonar = SonarQubeClient(sonarqube_url=url, username=username, password=password)

# set fieldnames to csv
fieldnames = ['name', 'bugs', 'code_smells', 'sqale_index', 'duplicated_blocks', 'duplicated_lines_density', 'complexity']

# get all projects on sonarqube
projects = list(sonar.projects.search_projects())

with open(f"../results/{filename}.csv", 'w', newline='') as file:
    # create a csv writer
    writer = csv.DictWriter(file, fieldnames = fieldnames)
    writer.writeheader()

    for p in projects:
        # get measures from sonarqube for project p
        component = sonar.measures.get_component_with_specified_measures(component=p['key'],
                                                                     fields="metrics,periods",
                                                                     metricKeys="code_smells,bugs,duplicated_lines_density,duplicated_blocks,sqale_index, complexity")

        if delete == 'yes':
            sonar.projects.delete_project(project=p['key'])

        # create project p dictionary with name and measures
        project = {'name': component['component']['name']}
        for measure in component['component']['measures']:
            if measure['metric'] == 'sqale_index':
                # Considering that a day has 8 hours of work
                project[measure['metric']] = round(int(measure['value']) / (60 * 8), 1)
            else:
                project[measure['metric']] = measure['value']
        # write project p to csv
        writer.writerow(project)
