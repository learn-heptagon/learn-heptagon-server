var Range = ace.require("ace/range").Range

const url = "http://localhost:8000/"

async function req() {
    let prog = editor.getValue();

    let response = await fetch(url + "verify", {
        method: "POST",
        body: JSON.stringify({
            prog: prog
        }),
        headers: {
            "Content-type": "application/json; charset=UTF-8"
        }
    });
    let results = await response.json();
    console.log(results);

    for(var i in results) {
        var o = results[i];
        if(o.objectType == "property" && o.source != "Generated") {
            console.log(o.line);
            let range = new Range (o.line, 0, o.line, 10);
            if(o.answer.value == "valid") {
                editor.session.addGutterDecoration(o.line-1, "valid-decoration");
            } else {
                editor.session.addGutterDecoration(o.line-1, "invalid-decoration");
            }
        }
    }

    // var results_res;
    // while(true) {
    //     // await new Promise(r => setTimeout(r, 1000));
    //     results_res = await fetch(url + "results", {
    //         method: "POST",
    //         body: JSON.stringify(jobid),
    //         headers:  {
    //             "Content-type": "application/json; charset=UTF-8"
    //         }
    //     })
    //     console.log(results_res);
    //     if(results_res.status != 503) break;
    // }
    // let results = await results_res.json();
    // console.log(results);
}
