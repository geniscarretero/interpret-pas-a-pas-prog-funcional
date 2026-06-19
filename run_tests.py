import subprocess
import difflib
import sys
import os

whole_file_tests = ["test_ast", "test_type"]
eval_test_size = 12


def check_whole_file_test(test, comanda, ruta_fitxer):

    # 1. Executar la comanda i capturar l'output
    try:
        # 'shell=True' permet executar comandes complexes.
        # 'text=True' fa que l'output es capturi com a text (String) i no com a bytes.
        resultat = subprocess.run(
            comanda, shell=True, capture_output=True, text=True, check=True
        )
        output_comanda = resultat.stdout
    except subprocess.CalledProcessError as e:
        print(
            f"Error en executar la comanda. Codi de retorn: {e.returncode}",
            file=sys.stderr,
        )
        print(f"Detalls de l'error:\n{e.stderr}", file=sys.stderr)
        return

    # 2. Llegir el fitxer de referència
    if not os.path.exists(ruta_fitxer):
        print(f"Error: El fitxer '{ruta_fitxer}' no existeix.", file=sys.stderr)
        print(
            "\nVols guardar l'output actual en aquest fitxer per a futures comparacions? (s/n)"
        )
        if input().lower() == "s":
            with open(ruta_fitxer, "w", encoding="utf-8") as f:
                f.write(output_comanda)
            print(f"Fitxer '{ruta_fitxer}' creat amb l'output actual.")
        return

    try:
        with open(ruta_fitxer, "r", encoding="utf-8") as f:
            contingut_fitxer = f.read()
    except Exception as e:
        print(f"Error en llegir el fitxer: {e}", file=sys.stderr)
        return

    # 3. Comparar i mostrar diferències
    if output_comanda == contingut_fitxer:
        print(f"\n✅ Test {test}: passat correctament.")
    else:
        print(f"\n❌ Test {test}: S'han trobat DIFERÈNCIES:\n")

        # Convertim el text en llistes de línies per a difflib
        linies_fitxer = contingut_fitxer.splitlines(keepends=True)
        linies_comanda = output_comanda.splitlines(keepends=True)

        # Generem un 'diff' unificat
        diferencies = difflib.unified_diff(
            linies_fitxer,
            linies_comanda,
            fromfile=f"Fitxer ({ruta_fitxer})",
            tofile="Output de la comanda",
        )

        # Mostrem el resultat (les línies amb '-' són al fitxer però no a la comanda,
        # i les línies amb '+' són a la comanda però no al fitxer)
        sys.stdout.writelines(diferencies)


def check_result_test(test, comanda, ruta_fitxer):
    # 1. Executar la comanda i capturar l'output
    try:
        # 'shell=True' permet executar comandes complexes.
        # 'text=True' fa que l'output es capturi com a text (String) i no com a bytes.
        resultat = subprocess.run(
            comanda, shell=True, capture_output=True, text=True, check=True
        )
        output_comanda = resultat.stdout
    except subprocess.CalledProcessError as e:
        print(
            f"Error en executar la comanda. Codi de retorn: {e.returncode}",
            file=sys.stderr,
        )
        print(f"Detalls de l'error:\n{e.stderr}", file=sys.stderr)
        return

    # 2. Llegir el fitxer de referència
    if not os.path.exists(ruta_fitxer):
        print(f"Error: El fitxer '{ruta_fitxer}' no existeix.", file=sys.stderr)
        print(
            "\nVols guardar l'output actual en aquest fitxer per a futures comparacions? (s/n)"
        )
        if input().lower() == "s":
            with open(ruta_fitxer, "w", encoding="utf-8") as f:
                f.write(output_comanda)
            print(f"Fitxer '{ruta_fitxer}' creat amb l'output actual.")
        return

    try:
        with open(ruta_fitxer, "r", encoding="utf-8") as f:
            contingut_fitxer = f.read()
    except Exception as e:
        print(f"Error en llegir el fitxer: {e}", file=sys.stderr)
        return
    # comparar última linea amb fitxer
    resultat_fitxer = contingut_fitxer.splitlines(keepends=True)[-1]
    resultat_comanda = output_comanda.splitlines(keepends=True)[-3]
    if resultat_fitxer == resultat_comanda:
        print(f"\n✅ Test {test}: passat correctament.")
    else:
        print(f"\n❌ Test {test}: S'han trobat DIFERÈNCIES:\n")

        print(
            f"S'esperava el resultat:\n{resultat_fitxer}\nS'ha obtingut:\n{resultat_comanda}\n"
        )


if __name__ == "__main__":
    # --- CONFIGURACIÓ ---
    # Escriu aquí la comanda que vols provar (ex: "ls -l" a Linux/Mac o "dir" a Windows)
    # També pots fer crides a altres scripts o programes, ex: "python un_altre_script.py"
    for whole_file_test in whole_file_tests:
        comanda = f"cabal run < ./tests/{whole_file_test}.hs"
        fitxer = f"./tests/{whole_file_test}.out"
        check_whole_file_test(whole_file_test, comanda, fitxer)
    # testos evaluació
    for eval_test_num in range(eval_test_size):
        comanda = f"cabal run < ./tests/test_eval_{eval_test_num}.hs"
        fitxer = f"./tests/test_eval_{eval_test_num}.out"
        check_result_test(f"test_eval_{eval_test_num}", comanda, fitxer)

