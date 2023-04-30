/***********/
/* CONSOLE */
/***********/

GW.console = {
    outputBuffer: newDocument(),

    flushBuffer: () => {
        if (GW.console.view == false)
            return;

        GW.console.view.contentView.append(GW.console.outputBuffer);
        GW.console.updateHeight();
        GW.console.scrollToBottom();
    },

    scrollToBottom: () => {
        GW.console.view.scrollView.scrollTop = GW.console.view.contentView.clientHeight
                                             - GW.console.view.scrollView.clientHeight;
    },

    clearOutput: () => {
        GW.console.view.contentView.replaceChildren();
        GW.console.updateHeight();
        GW.console.scrollToBottom();
    },

    print: (entity, flush = true) => {
        let style = "";
        if (   entity == undefined
            || entity == null)
            style = "color: #777;"
        if (entity instanceof Error)
            style = "color: #f00;"

        let output;
        if (entity instanceof Error) {
            output = entity.stack;
            console.error(entity);
        } else if (typeof entity == "string") {
            output = entity.replace(/</g, "&lt;").replace(/>/g, "&gt;");
            console.log(entity);
        } else if (entity instanceof Element) {
            output = entity.outerHTML.replace(/</g, "&lt;").replace(/>/g, "&gt;");
            console.log(entity);
        } else {
            if (entity) {
                let jsonString = JSON.stringify(entity, null, "\n"
                                      ).replace(/\\n/g, "\n"
                                      ).replace(/\\t/g, "\t"
                                      ).replace(/\\"/g, "\"")
                console.log(jsonString);
                jsonString = JSON.stringify(entity, null, "\n"
                                  ).replace(/\\t/g, "    "
                                  ).replace(/\\"/g, "&quot;"
                                  ).replace(/</g, "&lt;"
                                  ).replace(/>/g, "&gt;"
                                  ).replace(/\n+/g, "\n"
                                  ).replace(/\\n/g, "<br />")
                output = jsonString;
            } else {
                console.log(entity);
                output = entity;
            }
        }

        GW.console.outputBuffer.appendChild(newElement("P", { style: style }, { innerHTML: output }));

        if (flush)
            GW.console.flushBuffer();
    },

    setInputCursorPosition: (pos) => {
        GW.console.view.input.setSelectionRange(pos, pos);
    },

    show: () => {
        GW.console.scrollToBottom();
        GW.console.view.classList.toggle("hidden", false);
        GW.console.view.input.focus();
    },

    hide: () => {
        GW.console.view.input.blur();
        GW.console.view.classList.toggle("hidden", true);
    },

    isVisible: () => {
        return (GW.console.view.classList.contains("hidden") == false);
    },

    updateHeight: () => {
        GW.console.view.style.setProperty("--GW-console-view-height", GW.console.view.offsetHeight + "px");
    },

    setPrompt: (string) => {
        GW.console.view.prompt.innerHTML = string;
    },

    clearCommandLine: () => {
        GW.console.view.input.value = "";
    },

    keyDown: (event) => {
        if (GW.console.isVisible() == false)
            return;

        let allowedKeys = [ "Enter", "ArrowUp", "ArrowDown" ];
        if (allowedKeys.includes(event.key) == false)
            return;

        if (document.activeElement != GW.console.view.input)
            return;

        switch (event.key) {
            case "Enter":
                GW.console.commandLineCommandReceived();
                break;

            case "ArrowUp":
                event.preventDefault();
                if (GW.console.commandLog_pointer == GW.console.commandLog.length)
                    GW.console.commandLog_currentCommandLine = GW.console.view.input.value;
                let prevLine = GW.console.commandLog_prevEntry();
                if (prevLine != null) {
                    GW.console.view.input.value = prevLine;
                    GW.console.setInputCursorPosition(GW.console.view.input.value.length);
                }
                break;

            case "ArrowDown":
                event.preventDefault();
                let nextLine = GW.console.commandLog_nextEntry();
                if (nextLine != null) {
                    GW.console.view.input.value = nextLine;
                    GW.console.setInputCursorPosition(GW.console.view.input.value.length);
                }
                break;
        }
    },

    keyUp: (event) => {
        let allowedKeys = [ "`", "Esc", "Escape" ];
        if (allowedKeys.includes(event.key) == false)
            return;

        switch (event.key) {
            case "`":
                if (GW.console.isVisible() == false)
                    GW.console.show();
                break;
            case "Esc":
            case "Escape":
                if (GW.console.isVisible() == true)
                    GW.console.hide();
                break;
        };
    },

    commandLineInputReceived: (event) => {
        //  Nothing… yet.
    },

    commandLog: [ ],
    commandLog_currentCommandLine: "",
    commandLog_pointer: 0,
    commandLog_prevEntry: () => {
        if (GW.console.commandLog_pointer == 0)
            return null;

        return GW.console.commandLog_entryAtIndex(--(GW.console.commandLog_pointer));
    },
    commandLog_nextEntry: () => {
        if (GW.console.commandLog_pointer == GW.console.commandLog.length)
            return null;

        return GW.console.commandLog_entryAtIndex(++(GW.console.commandLog_pointer));
    },
    commandLog_entryAtIndex: (index) => {
        return (index == GW.console.commandLog.length
                ? GW.console.commandLog_currentCommandLine
                : GW.console.commandLog[index]);
    },

    commandLineCommandReceived: () => {
        let inputLine = event.target.value;

        GW.console.print("> " + inputLine);
        GW.console.clearCommandLine();

        GW.console.commandLog.push(inputLine);
        GW.console.commandLog_currentCommandLine = "";
        GW.console.commandLog_pointer = GW.console.commandLog.length;

        if (/^`.*`$/.test(inputLine))
            GW.console.jsExecLine(inputLine.slice(1, -1));
        else
            GW.console.execLine(inputLine);
    },

    execLine: (line) => {
        let command = line;
        switch (command.toLowerCase()) {
            case "clear":
                GW.console.clearOutput();
                break;
            default:
                GW.console.print(`gwrnsh: ${line}: command not found.`);
                break;
        }
    },

    jsExecLine: (line) => {
        $(line);
    }
};

//  Dump temporary buffer.
if (GW.consoleTempBuffer > "") {
    GW.consoleTempBuffer.split("\n").forEach(line => {
        GW.console.print(line, false);
    });
    GW.consoleTempBuffer = null;
}

doWhenBodyExists(() => {
    //  Construct views.
    GW.console.view = addUIElement(`<div id="console" class="hidden">
        <div class="console-scroll-view">
            <div class="console-content-view"></div>
        </div>
        <div class="console-command-line">
            <div class="console-command-line-prompt">
                <span></span>
            </div>
            <div class="console-command-line-entry-field">
                <input name="console-command" title="JS REPL" type="text" autocomplete="off"></input>
            </div>
        </div>
    </div>`);

    //  Convenience references.
    GW.console.view.scrollView = GW.console.view.querySelector(".console-scroll-view");
    GW.console.view.contentView = GW.console.view.querySelector(".console-content-view");
    GW.console.view.prompt = GW.console.view.querySelector(".console-command-line-prompt span");
    GW.console.view.input  = GW.console.view.querySelector(".console-command-line-entry-field input");

    //  Set prompt.
    GW.console.setPrompt(location.pathname);

    //  Flush output buffer.
    GW.console.flushBuffer();

    //  Update height.
    GW.console.updateHeight();

    //  Add event listeners, if console enabled.
    if (   getQueryVariable("console") == "1"
        || getQueryVariable("console") == "2"
        || localStorage.getItem("console-enabled") == "true") {
        //  Add show/hide key event listener.
        document.addEventListener("keyup", GW.console.keyUp);

        //  Add command line “Enter” key event listener.
        document.addEventListener("keydown", GW.console.keyDown);

        //  Add command line input (text entry) event listener.
        GW.console.view.input.addEventListener("input", GW.console.commandLineInputReceived);
    }

    //  Show console, if auto-show enabled.
    if (getQueryVariable("console") == "2")
        GW.console.show();
});



