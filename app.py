################################################################################
# Developed by Dr. Matthew A. Lanham
# Email: mlanham1@butler.edu
# Date: 12/17/2025
#
#your-project/
#├─ app.py
#├─ questions.xlsx
#└─ www/
#   ├─ Jeopardy-theme-song.mp3
#   ├─ daily-double.mp3
#   ├─ daily-double.jpg
#   └─ (any images referenced in the Question column, e.g., q3.jpg, lr1000.png)
################################################################################
# pip install shiny pandas openpyxl

from shiny import App, ui, render, reactive
import pandas as pd
import re
from pathlib import Path

# ---------- Paths ----------
BASE = Path(__file__).parent.resolve()
EXCEL_PATH = BASE / "questions.xlsx"
WWW_DIR = BASE / "www"

# ---------- Utilities ----------
def normalize_headers(cols):
    return [re.sub(r"[^a-z0-9]+", "_", str(c).strip().lower()) for c in cols]

def canon_value_str(x):
    try:
        n = float(x)
        return str(int(n)) if n.is_integer() else f"{n:g}"
    except Exception:
        return str(x).strip().lstrip("$")

def sheet_names():
    if not EXCEL_PATH.exists():
        return []
    try:
        return pd.ExcelFile(str(EXCEL_PATH)).sheet_names
    except Exception:
        return []

def read_sheet(sheet_name: str) -> pd.DataFrame:
    df = pd.read_excel(str(EXCEL_PATH), sheet_name=sheet_name, engine="openpyxl")
    df.columns = normalize_headers(df.columns)

    for req in ("category", "value", "question"):
        if req not in df.columns:
            raise ValueError(f"Missing required column '{req}' in sheet '{sheet_name}'.")

    df["category"] = df["category"].astype(str)
    df["value"] = df["value"].astype(str).str.replace("$", "", regex=False).str.strip()
    df["value_num"] = pd.to_numeric(df["value"], errors="coerce")
    df["value_str"] = df["value"].apply(canon_value_str)
    df["value_lab"] = "$" + df["value_str"]
    df["question"] = df["question"].astype(str).str.strip()
    df["key"] = df["category"] + "_" + df["value_lab"]

    if "daily_double" not in df.columns:
        df["daily_double"] = 0
    df["daily_double"] = pd.to_numeric(df["daily_double"], errors="coerce").fillna(0).astype(int)
    return df

def pick_asset(candidates):
    if not WWW_DIR.exists():
        return None
    for pat in candidates:
        for p in WWW_DIR.glob(pat):
            if p.is_file():
                return p.name
    return None

# Accept common variants; if nothing is found we’ll handle gracefully
DAILY_DOUBLE_IMG = pick_asset(
    ["daily-double.jpg", "daily_double.jpg", "Daily*Double*.jpg",
     "daily-double.jpeg", "daily_double.jpeg", "Daily*Double*.jpeg",
     "daily-double.png", "daily_double.png", "Daily*Double*.png"]
)
DAILY_DOUBLE_MP3 = pick_asset(["daily-double.mp3", "daily_double.mp3", "Daily*Double*.mp3"])
THEME_MP3        = pick_asset(["Jeopardy-theme-song.mp3", "jeopardy*theme*.mp3"])

# Convenience URLs (only if files exist locally)
def url_if_exists(fname):
    return f"/static/{fname}" if fname and (WWW_DIR / fname).exists() else None

DAILY_DOUBLE_IMG_URL = url_if_exists(DAILY_DOUBLE_IMG)
DAILY_DOUBLE_MP3_URL = url_if_exists(DAILY_DOUBLE_MP3)
THEME_MP3_URL        = url_if_exists(THEME_MP3)

# ---------- Styles & JS ----------
CSS = ui.tags.style("""
body { background:black; } h2 { color:white; }
.topbar { width:90%; margin:18px auto 0; display:flex; gap:10px; align-items:center; }
.spacer { flex:1 1 auto; }
.board { display:grid; grid-template-columns:repeat(6,1fr); gap:5px; width:90%; margin:12px auto 0; }
.category { background:#081A6B; color:white; font-weight:bold; font-size:22px; text-align:center; padding:10px; border:2px solid black; }
.tile { background:#081A6B; color:#FFCC00; font-weight:bold; font-size:26px; text-align:center; padding:25px; border:2px solid black; cursor:pointer; user-select:none; }
.tile.black { background:black; color:black; }
.tile.disabled { background:#222; color:#555; cursor:not-allowed; }
.btn-theme { background:#1e3a8a; color:#fff; font-weight:600; border:none; padding:8px 12px; border-radius:6px; }
.clue-wrap { background:#0b2d84;
  background:radial-gradient(ellipse at center,#1546b4 0%,#0b2d84 45%,#071a5e 100%);
  padding:48px 36px; text-align:center; }
.clue-text { color:#fff; text-shadow:2px 2px 0 #000; font-family:Georgia,'Times New Roman',serif;
  font-weight:bold; letter-spacing:1px; font-size:38px; line-height:1.25; text-transform:uppercase; }
""")

JS = ui.tags.script("""
Shiny.addCustomMessageHandler('play_theme', function(_){
  var t=document.getElementById('theme'); if(t){ try{ t.currentTime=0; t.play(); }catch(e){} }
});
Shiny.addCustomMessageHandler('stop_theme', function(_){
  var t=document.getElementById('theme'); if(t){ try{ t.pause(); t.currentTime=0; }catch(e){} }
});
Shiny.addCustomMessageHandler('play_dd', function(_){
  var a=document.getElementById('ddsound'); if(a){ try{ a.currentTime=0; a.play(); }catch(e){} }
});
$(document).on('hidden.bs.modal','#shiny-modal',function(){
  Shiny.setInputValue('modal_closed', Date.now(), {priority:'event'});
});
""")

# ---------- UI ----------
audio_tags = []
if DAILY_DOUBLE_MP3_URL:
    audio_tags.append(ui.tags.audio(id="ddsound", src=DAILY_DOUBLE_MP3_URL, type="audio/mpeg", preload="auto"))
if THEME_MP3_URL:
    audio_tags.append(ui.tags.audio(id="theme", src=THEME_MP3_URL, type="audio/mpeg", preload="auto"))

app_ui = ui.page_fluid(
    ui.tags.head(CSS, JS),
    *audio_tags,
    ui.h2("Jeopardy Review", align="center"),
    ui.div(
        {"class": "topbar"},
        ui.input_select(
            "sheet", "Question set (sheet):",
            choices=sheet_names(),
            selected=(sheet_names()[0] if sheet_names() else None),
            width="300px",
        ),
        ui.div({"class": "spacer"}),
        ui.input_action_button("play_theme", "Play Theme", class_="btn-theme"),
        ui.input_action_button("stop_theme", "Stop Theme", class_="btn-theme"),
    ),
    ui.output_ui("board_ui")
)

# ---------- Server ----------
def server(input, output, session):
    last_clicked = reactive.Value(None)

    # One-time toast so you can see what the app can serve from /static
    @reactive.Effect
    def _static_listing():
        if WWW_DIR.exists():
            names = ", ".join(sorted(p.name for p in WWW_DIR.glob("*") if p.is_file())) or "(none)"
            ui.notification_show(f"static /www -> {WWW_DIR}\nFiles: {names}", duration=6)

    @reactive.Calc
    def df():
        sheets = sheet_names()
        sheet = input.sheet() or (sheets[0] if sheets else None)
        if sheet is None:
            return pd.DataFrame(columns=["category","value_lab","key","daily_double"])
        try:
            return read_sheet(sheet)
        except Exception as e:
            ui.notification_show(f"Error reading '{EXCEL_PATH.name}': {e}", type="danger", duration=8)
            return pd.DataFrame(columns=["category","value_lab","key","daily_double"])

    @reactive.Calc
    def categories():
        return pd.unique(df()["category"]).tolist()

    @reactive.Calc
    def value_labels():
        if df().empty:
            return []
        v = df()[["value_num","value_lab"]].drop_duplicates().sort_values("value_num", ascending=True)
        return v["value_lab"].tolist()

    tile_states = reactive.Value({})

    @reactive.Effect
    def reset_states():
        _ = (categories(), value_labels())
        tile_states.set({f"{cat}_{lab}": False for cat in categories() for lab in value_labels()})

    @reactive.Calc
    def dd_key():
        if df().empty:
            return None
        flagged = df().loc[df()["daily_double"] == 1, "key"].tolist()
        if len(flagged) == 0:
            return None
        if len(flagged) > 1:
            ui.notification_show("Multiple Daily Double cells flagged; using the first one.", type="warning", duration=5)
        return flagged[0]

    def get_clue(tile_id: str):
        row = df().loc[df()["key"] == tile_id]
        if row.empty:
            return {"type": "text", "payload": "(No question found in spreadsheet for this tile.)"}
        q = str(row["question"].iloc[0]).strip()
        if re.search(r"\.(png|jpe?g|gif)$", q, flags=re.IGNORECASE) and (WWW_DIR / q).exists():
            return {"type": "image", "payload": f"/static/{q}"}
        return {"type": "text", "payload": q}

    def show_clue_modal(clue: dict):
        if clue["type"] == "image":
            ui.modal_show(ui.modal(
                ui.div({"class": "clue-wrap"},
                       ui.img(src=clue["payload"], style="max-width:100%; height:auto; border:6px solid #ffcc00; box-shadow:0 0 20px #000;")),
                easy_close=True, footer=None, size="l"
            ))
        else:
            ui.modal_show(ui.modal(
                ui.div({"class": "clue-wrap"},
                       ui.div(clue["payload"], class_="clue-text")),
                easy_close=True, footer=None, size="l"
            ))

    @reactive.Effect
    @reactive.event(input.play_theme)
    async def _play_theme():
        # Only try to play if the audio tag exists (file found)
        if THEME_MP3_URL:
            await session.send_custom_message("play_theme", None)
        else:
            ui.notification_show("Theme audio not found in /www.", duration=4)

    @reactive.Effect
    @reactive.event(input.stop_theme)
    async def _stop_theme():
        if THEME_MP3_URL:
            await session.send_custom_message("stop_theme", None)

    @reactive.Effect
    @reactive.event(input.tile_click)
    async def _on_tile_click():
        tile_id = input.tile_click()
        if not tile_id:
            return
        last_clicked.set(tile_id)

        present = (df()["key"] == tile_id).any()
        if not present:
            return

        if dd_key() and tile_id == dd_key():
            if DAILY_DOUBLE_IMG_URL:
                banner = ui.img(src=DAILY_DOUBLE_IMG_URL, style="width:100%; height:auto;")
            else:
                banner = ui.h3("DAILY DOUBLE!", style="color:#FFCC00; font-weight:bold; text-align:center;")
            ui.modal_show(ui.modal(
                ui.div(
                    banner,
                    ui.h3("DAILY DOUBLE!", style="color:#FFCC00; font-weight:bold; margin:12px 0; text-align:center;"),
                    style="background:#000; text-align:center;"
                ),
                easy_close=True, footer=None, size="l"
            ))
            if DAILY_DOUBLE_MP3_URL:
                await session.send_custom_message("play_dd", None)
            else:
                ui.notification_show("Daily Double audio not found in /www.", duration=4)
        else:
            show_clue_modal(get_clue(tile_id))

        st = dict(tile_states.get())
        st[tile_id] = not st.get(tile_id, False)
        tile_states.set(st)

    @reactive.Effect
    @reactive.event(input.modal_closed)
    def _after_modal_closed():
        if last_clicked.get() and dd_key() and last_clicked.get() == dd_key():
            show_clue_modal(get_clue(dd_key()))

    @output
    @render.ui
    def board_ui():
        if not categories() or not value_labels():
            return ui.div({"style":"color:white; width:90%; margin:20px auto;"},
                          f"No data found. Ensure '{EXCEL_PATH.name}' is beside app.py and has Category, Value, Question columns.")
        items = []
        for cat in categories():
            items.append(ui.div(cat, class_="category"))
        for lab in value_labels():
            for cat in categories():
                tile_id = f"{cat}_{lab}"
                present = (df()["key"] == tile_id).any()
                classes = "tile" if present else "tile disabled"
                if present and tile_states.get().get(tile_id, False):
                    classes = "tile black"
                label = lab if (present and not tile_states.get().get(tile_id, False)) else ""
                onclick = f"Shiny.setInputValue('tile_click','{tile_id}',{{priority:'event'}})" if present else ""
                items.append(ui.div(label, id=tile_id, class_=classes, onclick=onclick))
        return ui.div({"class":"board"}, *items)

# Mount /static explicitly to the local ./www directory.
# IMPORTANT (OneDrive): Right-click the 'www' folder AND the media files inside it → “Always keep on this device”.
app = App(app_ui, server, static_assets={"/static": str(WWW_DIR)})
