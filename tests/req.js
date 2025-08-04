const url = "http://localhost:8000/"
const prog = "node s (_:bool) returns (X: int);\nlet\n  X = 1 -> if (pre X) < 3 then (pre X) + 1 else 3 ;\ntel\n\nnode s1 (_:bool) returns (X:  int);\nlet\n  X = 1 -> pre (2 -> 3);\ntel\nnode obs_s(_:bool) returns (OK: bool);\nlet\n  OK = (s(_) = s1(_));\n\n--%MAIN;\n--%PROPERTY OK;\ntel"

async function req() {
    console.log(prog);
    let response = await fetch(url + "verify", {
        method: "POST",
        body: JSON.stringify({
            prog: prog
        }),
        headers: {
            "Content-type": "application/json; charset=UTF-8"
        }
    });
    let resuls = await response.json();
    console.log(jobid);

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
