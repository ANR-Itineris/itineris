from pathlib import Path
from html import escape

import pandas as pd
import networkx as nx
from pyvis.network import Network


def clean_value(value) -> str:
    if pd.isna(value):
        return ""
    return str(value)


def normalize_identifier(value) -> str:
    """Normalize IDs so matching works reliably across CSV/XLSX sources."""
    if pd.isna(value):
        return ""

    if isinstance(value, float):
        if value.is_integer():
            value = int(value)
        else:
            value = format(value, "f").rstrip("0").rstrip(".")

    return str(value).strip().replace(",", ".")


def create_method_network(
    data_file: str = "idfs_table_VIEW_ONLY.csv",
    typochrono_file: str = "Extrait_BDD_sites_Itineris_06_2026.xlsx",
    output_html: str = "idfs_network.html",
    notebook: bool = False,
) -> str:

    base_dir = Path(__file__).parent
    data_path = base_dir / "archaeometry" / data_file
    typochrono_path =  Path(__file__) / typochrono_file
    output_html_path = base_dir / output_html

    df = pd.read_csv(data_path)

    method_cols = [
        "icp", "lia", "metallo", "pixe", "raman",
        "sem_eds", "tomo", "xrf", "xr"
    ]

    id_cols = ["n_iti", "n_man", "n_c2rmf", "n_mrt", "n_re"]

    G = nx.Graph()

    # ---------------------------------------------------------------------
    # 1. Existing object and method nodes from idfs_table_VIEW_ONLY.csv
    # ---------------------------------------------------------------------
    for _, row in df.iterrows():
        iti_full = normalize_identifier(row.get("n_iti"))

        if not iti_full or iti_full.upper() == "NA":
            continue

        iti_label = iti_full.removeprefix("ITI_")

        popup_html = "<h3>{}</h3>".format(escape(iti_full))
        popup_html += "<table>"

        for col in id_cols:
            popup_html += (
                "<tr>"
                f"<td><b>{escape(col)}</b></td>"
                f"<td>{escape(clean_value(row.get(col)))}</td>"
                "</tr>"
            )

        popup_html += "</table>"

        G.add_node(
            iti_full,
            label=iti_label,
            group="object",
            popup=popup_html,
        )

        for method in method_cols:
            if method in df.columns and row.get(method) == 1:
                G.add_node(
                    method,
                    label=method.upper(),
                    group="method",
                )
                G.add_edge(iti_full, method)

    # ---------------------------------------------------------------------
    # 2. New typochrono/site nodes from Extrait_BDD_sites_Itineris_06_2026.xlsx
    #    Matching key: XLSX column 'id' == CSV column 'n_iti'
    #    Node label: XLSX column 'denomination::terme_fr'
    # ---------------------------------------------------------------------
    if typochrono_path.exists():
        df_typochrono = pd.read_excel(typochrono_path)

        required_cols = {"id", "denomination::terme_fr"}
        missing_cols = required_cols - set(df_typochrono.columns)
        if missing_cols:
            raise ValueError(
                f"Missing column(s) in {typochrono_path}: "
                f"{', '.join(sorted(missing_cols))}"
            )

        for _, row in df_typochrono.iterrows():
            site_id = normalize_identifier(row.get("id"))

            if not site_id or site_id.upper() == "NA":
                continue

            denomination = clean_value(row.get("denomination::terme_fr")).strip()
            site_label = denomination if denomination else site_id
            site_node_id = f"site::{site_id}"

            popup_html = "<h3>{}</h3>".format(escape(site_label))
            popup_html += "<table>"
            popup_html += (
                "<tr>"
                "<td><b>id</b></td>"
                f"<td>{escape(site_id)}</td>"
                "</tr>"
            )
            popup_html += (
                "<tr>"
                "<td><b>denomination::terme_fr</b></td>"
                f"<td>{escape(denomination)}</td>"
                "</tr>"
            )
            popup_html += "</table>"

            G.add_node(
                site_node_id,
                label=site_label,
                group="typochrono",
                popup=popup_html,
            )

            # Link to the existing object node if the ID is present in idfs_table_VIEW_ONLY.csv
            if G.has_node(site_id):
                G.add_edge(site_node_id, site_id, label="")
    else:
        print(f"Warning: typochrono file not found: {typochrono_path}")

    net = Network(
        height="750px",
        width="100%",
        bgcolor="#ffffff",
        font_color="black",
        notebook=notebook,
        cdn_resources="in_line",
    )

    net.from_nx(G)

    for node in net.nodes:
        if node.get("group") == "object":
            node["shape"] = "circle"
            node["size"] = 22
            node["font"] = {
                "size": 14,
                "align": "center",
            }

        elif node.get("group") == "method":
            node["shape"] = "box"
            node["size"] = 20

        elif node.get("group") == "typochrono":
            node["shape"] = "ellipse"
            node["size"] = 18
            node["font"] = {
                "size": 13,
                "align": "center",
            }

    # net.show_buttons(filter_=["physics"])

    print(f"Saving network visualization to: {output_html_path}")

    html = net.generate_html(notebook=notebook)
    output_html_path.write_text(html, encoding="utf-8")

    inject_click_popup(output_html_path)

    return str(output_html_path)


def inject_click_popup(html_path: Path) -> None:
    html = html_path.read_text(encoding="utf-8")

    popup_code = """
<style>
#node-popup {
    position: fixed;
    top: 20px;
    right: 20px;
    min-width: 280px;
    max-width: 420px;
    background: white;
    border: 1px solid #999;
    border-radius: 8px;
    padding: 12px;
    box-shadow: 0 3px 12px rgba(0,0,0,0.25);
    font-family: Arial, sans-serif;
    z-index: 9999;
    display: none;
}

#node-popup h3 {
    margin-top: 0;
}

#node-popup table {
    border-collapse: collapse;
    width: 100%;
}

#node-popup td {
    padding: 4px 6px;
    border-bottom: 1px solid #eee;
}

#node-popup-close {
    float: right;
    cursor: pointer;
    font-weight: bold;
}
</style>

<div id="node-popup">
    <span id="node-popup-close">×</span>
    <div id="node-popup-content"></div>
</div>

<script>
network.on("click", function(params) {
    if (params.nodes.length > 0) {
        var nodeId = params.nodes[0];
        var node = nodes.get(nodeId);

        if (node.popup) {
            document.getElementById("node-popup-content").innerHTML = node.popup;
            document.getElementById("node-popup").style.display = "block";
        }
    }
});

document.getElementById("node-popup-close").onclick = function() {
    document.getElementById("node-popup").style.display = "none";
};
</script>
"""

    html = html.replace("</body>", popup_code + "\n</body>")
    html_path.write_text(html, encoding="utf-8")


if __name__ == "__main__":
    create_method_network(
        data_file="idfs_table_VIEW_ONLY.csv",
        typochrono_file="../Extrait_BDD_sites_Itineris_06_2026.xlsx",
        output_html="idfs_network_test.html",
    )
