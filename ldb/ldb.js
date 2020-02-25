// Get trace file and source file from GET parameters
let url = new URL(window.location.href);
var debug = url.searchParams.has("debug");

// 'trace' will hold objects { ip, regs, memwrite, memread }
// ip : instruction pointer
// regs : register values, e.g. { s0: 12, sp: 9888, a0: 1, ... }
// memwrite : memory bytes written at this step [ { addr: 9809, v: 0x00-0xff }, ... ]
// memread : list of memory addresses read at this step
// output : output at that step
var trace = {};

// current_step : index in `trace` array
var current_step = 0;

// current_label : funname_instrindex
var current_label = undefined;

// set of addresses to display -- populated by get_interesting_addresses() based
// on addresses accessed in the trace
var interesting_addresses = [];

// set of breakpoints (funname_instrindex)
var breaks = [];

// for each function, its start and end addresses in the code
var funboundaries = {};

// Expressions are evaluated in the current state. Memory subexpressions are
// identified and assigned a CSS class taken from the `available_css_classes`
// set. `expr_css_classes` records the relationship between classes and
// expressions.
let available_css_classes = ["expr1", "expr2", "expr3", "expr4"];
var expr_css_classes = {};

// List of all registers. Used for showing the state of the registers, in that
// order in a 8-col x 4-row table.
let all_regs = [ "zero" , "ra" , "sp" , "gp" , "s0", "s1"  , "s2"  , "s3"  ,
                 "s4"   , "s5" , "s6" , "s7" , "s8" , "s9" , "s10" , "s11" ,
                 "a0"   , "a1" , "a2" , "a3" , "a4" , "a5" , "a6"  , "a7"  ,
                 "t0"   , "t1" , "t2" , "t3" , "t4" , "t5" , "t6",  "tp" ];


// Table which holds the value of any memory address at each step. Populated
// lazily by the membyte function.
var memo_table = {};

var cfgNodes;
var cfgEdges;
var cfgNetwork;
var curCfgNode;
var nodeId = {};

// Variable for websocket -- initialized later, once all data has been loaded
var ws;

var log = {};

// Have we received data for step 'step'?
function trace_for_step(step){
    return Object.keys(trace).includes(String(step));
}

// Send a json object to the debugger
function send_to_debugger(json){
    console.log("Sending "+ JSON.stringify(json));
    ws.send(JSON.stringify(json)+"\n");
}

// Conversion to hex
// One byte
function byte_to_string(i){
    return ("00" + i.toString(16)).substr(-2);
}
// One word
function word_to_string(i){
    return ("0".repeat(8) + i.toString(16)).substr(-8);
}
// A bitvector of sz hex digits (hexvector?)
function bv_to_string(sz, i){
    return ("0".repeat(sz) + i.toString(16)).substr(-sz);
}

// Get ASCII character corresponding to code i if printable, '.' otherwise
function byte_to_char(i){
    let v = String.fromCharCode(i);
    return v.replace(/[^\x20-\x7E]/g, '.');;
}

var isPlaying = false;
var playSpeed = 0.6;
var isFinished = false;
var timer = undefined;


function toggle(elt, icon) {
    var isHidden = elt.style('display') == 'none';
    if(isHidden){
        icon.html("-");
        return elt.style('display', 'inherit');
    } else {
        icon.html("+");
        return elt.style('display', 'none');
    }
}

// Given a list of highlighted ranges of addresses 'highlight', determine wheter
// 'addr' is in one of these ranges. A range is a JS object r with r.addr giving
// the range start address and r.sz its size
// Returns an array a where a[0] is a boolean saying whether or not it should be
// highlighted, and a[1] is the CSS class that should be applied
function is_highlighted(addr, highlight){
    return highlight.reduce(function (acc,e) {
        if (acc[0]) return acc;
        if (e.addr <= addr && addr < e.addr + e.sz){
            return [true, e.expr];
        }
        return acc;
    }, [false, ""]);
}

// Display the memory at a given step, from memory address 'start' and
// 'numbytes' bytes on. Highlight those addresses that are in highlight.
function show_mem(step, start, numbytes, highlight){
    var s = "";
    var line = []; // holds the bytes in the line currently under construction,
                   // used for printing chars on the right of each line
    for(var j = 0; j < numbytes; j++){
        let addr = start + j;
        if(addr % 16 == 0) {
            s+="\n" + word_to_string(addr) + ":";
        }
        if(addr %8 == 0) s+=" ";
        let v = membyte(step, addr);
        line.push(v);
        let touched = (Object.keys(trace[step].memwrite).includes(addr.toString())) ? "touched" : "untouched";
        let read = (trace[step].memread.includes(addr)) ? "read" : "unread";

        let hi = is_highlighted(addr, highlight);
        var col = "";
        if(hi[0]){
            col = expr_css_classes[hi[1]] ;
        }
        s+= "<span class=\""+ touched + " "+ read + " " + col +"\">" + byte_to_string(v) + "</span> ";
        if((addr + 1) % 16 == 0) {
            line.forEach(function(i){
                s+= byte_to_char(i);
            });
            line = [];
        }
    }
    return s;
}

// Show the state at step 'step'. 'highlight' is the set of addresses that
// should be highlighted, i.e. those that appears in expressions that we watch.
// Shows registers, memory at "interesting addresses" (computed in
// get_interesting_addresses).
function show_state(step, highlight){
    let numcols = 4;
    var s = "<h1>Registers</h1><table>";
    let regs = trace[step]["regs"];
    all_regs.forEach(function (k, i){
        if (i % numcols == 0) s+= "<tr>";
        s += "<td class=\"regname\">" + k
            + "</td><td class=\"regval\" title=\"" + regs[k] + "\">0x"+ word_to_string(regs[k]) +"</td>";
        i++;
        if (i % numcols == 0) s+= "</tr>";
    });
    s += "</table>";
    document.querySelector('#regstate').innerHTML = s;
    // s = "<h1>Stack</h1><pre>";
    // let sp = regs["sp"];
    // let s0 = regs["s0"];
    // for(var addr = sp; addr <= 10000; addr+=8){
    //     let a = Array.from(Array(8)).map((e,i) => membyte(step, addr + i));
    //     let v = a.reduce(function(acc, b, i) { return acc + b * (1 << (8*i)); }, 0);
    //     s += word_to_string(addr) + ": " + word_to_string(v) + "<br>";
    // }
    s = "<h1>Memory</h1><pre>";
    get_interesting_addresses();
    interesting_addresses.forEach(function (ad){
        s += "<hr>";
        s += show_mem(step, ad[0], ad[1], highlight);
    });

    document.querySelector('#memstate').innerHTML = s + "</pre>";
}

// Set current label to 'label' in the code view and make it visible
function set_current_label(label){
    if(current_label !== undefined){
        d3.select('#code_'+current_label).classed("current", false);
    }
    d3.select('#code_'+label).classed("current", true);
    var elt = document.getElementById('code_'+label);
    elt.scrollIntoView({block: "nearest"});
    current_label = label;
}


// Sets the state to be at step 'step'.
function state(step){
    // Number in navigator bar
    document.querySelector('#curstep').innerHTML = step;
    let t = trace[step];
    let label = t["ip"];
    let highlight = compute_expr(step);
    if(label in nodeId){
        if (curCfgNode !== undefined){
            cfgNodes.update([{id: nodeId[curCfgNode], group: ip_to_fun(curCfgNode)}]);
        }
        cfgNodes.update([{ id: nodeId[label], group: 'current'}]);
        curCfgNode = label;
    }
    
    display_exprs_and_logs(step);
    show_state(step, highlight);

    // Compute expressions
    let show_exprs = d3.select('#subexprs');
    show_exprs.html("");
    highlight.forEach(function (hi){
        show_exprs.insert("p")
            .html(hi.expr + " -> " + hi.val)
            .attr("title", bv_to_string(hi.sz, hi.val))
            .classed(expr_css_classes[hi.expr], true);
    });

    // Show output
    d3.select('#output').html("<h1>Output</h1><pre>" +  get_output(step) +"</pre>");
    // Set current label and step
    set_current_label(label);
    current_step = step;
}

// Add a breakpoint at a given label (+ communicates with debugger)
function add_breakpoint(label){
    label = parseInt(label);
    var i = breaks.indexOf(label);
    if(i > -1){
        breaks.splice(i, 1);
        send_to_debugger({'cmd': 'rmbreak', 'ip': label});
        d3.select('#code_'+label).classed("break", false);
    } else {
        breaks.push(label);
        send_to_debugger({'cmd': 'break', 'ip': label});
        d3.select('#code_'+label).classed("break", true);
    }
}

// Determines whethere a step is a breakpoint or not.
function is_step_breakpoint(step){
    return breaks.includes(trace[step]["ip"]);
}

// Shows a memdiff -- used during debug.
function string_of_memdiff(t){
    var s = "{";
    Object.keys(t).forEach(function (k){
        s+= " " + k + " -> " + t[k] + ",";
    });
    return s + "}";
}

// Gets the byte value at memory address 'addr' for step 'step'. If lucky, this
// is alread memoized in 'memo_table'. Otherwise, walk back the trace to find
// out, and saves the result in the 'memo_table'.
function membyte(step, addr){
    if (step in memo_table){
        if (addr in memo_table[step]){
            return memo_table[step][addr];
        }
    } else {
        memo_table[step] = {};
    }
    let t = trace[step].memwrite;
    if (Object.keys(t).includes(addr.toString())){
        memo_table[step][addr] = t[addr];
        return t[addr];
    } else {
        if (step == 0) { return "XX"; }
        else {
            let v = membyte(step-1, addr);
            memo_table[step][addr] = v;
            return v;
        }
    }
}


var out_table = {};
// Gets the output for step 'step'. 
function get_output(step){
    if (step in out_table){
        return out_table[step];
    }
    let t = trace[step].output;
    if (step == 0) {
        out_table[step] = t;
        return t;
    }
    else {
        let prev_output = get_output(step-1);
        let o = prev_output + t;
        out_table[step] = o;
        return o;
    }
}


// Initializes the debugger with parameter values.
function init(){
    var params = d3.select('#init_params').property('value');
    let p = params.split(" ").map((val) => parseInt(val));
    trace = [];
    breaks = [];
    memo_table = {};
    log = {};
    send_to_debugger({'cmd': 'init', memsize:10000, params: p});
    isFinished = false;
}

// Quits the debugger
function quit(){
    send_to_debugger({'cmd': 'quit'});
}

// Walk back one step, if possible
function prev_step(){
    if(current_step <= 0) return;
    current_step--;
    state(current_step);
}

// Go to next step, call debugger if necessary. Note: if the debugger is called,
// the current_step will be updated with the response from the debugger, not
// before.
function next_step(){
    if(!trace_for_step(current_step + 1)){
        if ( !isFinished)
            send_to_debugger({"cmd": "next"});
        else
            pause();
    } else {
        current_step++;
        state(current_step);
    }
}

// Go to next breakpoint, call debugger if necessary. Note: if the debugger is called,
// the current_step will be updated with the response from the debugger, not
// before.
function next_bp(){
    current_step++;
    if(trace_for_step(current_step)){
        if (is_step_breakpoint(current_step)){
            state(current_step);
            return;
        } else {
            next_bp();
        }
    } else if (! isFinished ){
        send_to_debugger({"cmd": "next_bp"});
    }
}

// Go to previous breakpoint, if possible. If step is unavailable, something has
// gone wrong (shouldn't happen).
function prev_bp(){
    if(current_step <= 0) {
        state(0);
        return;
    }
    current_step--;
    if(trace_for_step(current_step)){
        if(is_step_breakpoint(current_step)){
            state(current_step);
            return;
        } else {
            prev_bp();
        }
    } else {
        console.log('Lost intermediate states...');
        return;
    }
}

function ip_to_fun(ip){
    for (let [fname, bounds] of Object.entries(funboundaries)){
        if(bounds.start <= ip && ip < bounds.end){
            return fname;
        }
    }
    return "unknown";
}

function get_jumps(){
    var s = new Set();
    var curip = undefined;
    var states = [];
    trace.forEach(function (t){
        let first = curip === undefined;
        let next_label = first || (t.ip === curip+1);
        if(!next_label){
            states.push(curip);
        }
        curip = t.ip;
        if(first || !next_label)
            states.push(curip);
    });
    var states2 = [];
    trace.forEach(function (t){
        if(states.includes(t.ip)){ states2.push(t.ip); }
    });
    states = states2;
    nodeId = {};
    var nextId = 0;
    var nodes = [];
    states.forEach(function (s){
        if (!(s in nodeId)){
            nodes.push({ 'id': nextId, 'label' : String(s), group: ip_to_fun(s) });
            nodeId[s] = nextId;
            nextId++;
        }
    });
    var edges = [];
    var curs = undefined;
    states.forEach(function (s){
        if(curs !== undefined){
            let from = nodeId[curs];
            let to = nodeId[s];
            let f = edges.findIndex(e => e.from == from && e.to == to);
            var c = 1;
            if (f > -1){
                c = 1 + Number(edges[f].label);
                edges.splice(f,1);
            }
            edges.push({'from': from, 'to': to, 'arrows': 'to',
                        'physics': false, 'smooth': {'type': 'cubicBezier'}, 'label': String(c)});
        }
        curs = s;
    });
    let container = document.querySelector('#cfg_cont');
    var options = {
        layout: {
            randomSeed: 1,
        },
        edges:{
            color:{
                inherit:false
            }
        },
        physics: {
            forceAtlas2Based: {
                gravitationalConstant: -26,
                centralGravity: 0.005,
                springLength: 230,
                springConstant: 0.18
            },
            maxVelocity: 146,
            solver: "forceAtlas2Based",
            timestep: 0.35,
            stabilization: { iterations: 150 }
        },
        manipulation: false,
    };
    cfgNodes = new vis.DataSet(nodes);
    cfgEdges = new vis.DataSet(edges);
    let data = {
        nodes: cfgNodes,
        edges: cfgEdges
    };
    cfgNetwork = new vis.Network(container, data, options);
    d3.select('#cfg_legend').html("");
    for(let [g,info] of Object.entries(cfgNetwork.groups.groups)){
        d3.select('#cfg_legend')
            .insert("span")
            .style("background", info.color.background)
            .html(g)
            .insert("br");
    }
}

// Computes a set of memory addresses that are either read from or written to in
// the current trace. Outputs a list of memory regions that contain interesting
// addresses. Several distinct regions are output if no interesting addresses
// appear in allowable_skip*16 bytes.
function get_interesting_addresses(){
    var s = new Set();
    for (let [step, t] of Object.entries(trace)){
        Object.keys(t.memwrite).forEach((x) => s.add(Math.floor(x / 16)));
        t.memread.forEach((x) =>s.add(Math.floor(x / 16)));
    }
    var a = Array.from(s);
    a.sort((x,y)=>x-y); // to sort as numbers... WTF...
    var b = [];
    var curlo = undefined;
    var curhi = undefined;
    var allowable_skip = 4;
    a.forEach(function(x){
        if(curhi === undefined){
            curlo = x; curhi = x;
        } else {
            if (x - curhi <= allowable_skip){
                curhi = x;
            } else {
                b.push([curlo, curhi]);
                curlo = x; curhi = x;
            }
        }
    });
    if(curlo !== undefined) b.push([curlo, curhi]);
    interesting_addresses = b.map(x => [x[0]*16,(x[1]-x[0]+1)*16]);
}

function memstr(step, addr){
    var s = "";
    var c = membyte(step, addr);
    while(c !== 0){
        s += byte_to_char(c);
        addr++;
        c = membyte(step, addr);
    }
    return s;
}


function safeDictGet(dict, key, deft){
    key = key.toString();
    if(Object.keys(dict).includes(key))
        return dict[key];
    return deft;
}

function log2string(step){
    var s = "";
    for(var i = 0; i <= step; i++){
        s += safeDictGet(log, i, "");
    }
    d3.select('#log').html("<pre>"+s+"</pre>");
}

var expr_result = "";

function compute_expr (step){
    let newValue = d3.select('#expr_input').property("value");
    let ast = jsep(newValue);
    var hi = [];
    let env = {
        "env": {...trace[step].regs, "ip": trace[step].ip, "step": step},
        "mem": function(addr) { return membyte(step, addr); },
        "str": function(addr) { return memstr(step, addr); },
        "log": function(str) {
            if (!(Object.keys(log)).includes(step)){
                log[step] = "";
            }
            log[step] += str;
        },
        "memrecord": function(str, addr, sz, v) {
            if(!(str in expr_css_classes)){
                expr_css_classes[str] = available_css_classes[Object.keys(expr_css_classes).length % available_css_classes.length];
            }
            hi.push(
                {
                    'addr': addr,
                    'sz': sz,
                    'expr': str,
                    'val' : v
                }
            );
        }
    };
    expr_result = do_eval(ast, env);
    return hi;
}

function display_exprs_and_logs(step){
    log2string(step);
    d3.select('#expr_res').html(expr_result);
}


function play_next(){
    next_step();
    if (is_step_breakpoint(current_step)) {
        pause();
    }
}

function play(){
    isPlaying = true;
    d3.select('#play').style("font-weight", "bold");
    d3.select('#pause').style("font-weight", "normal");
    timer = setInterval(play_next, playSpeed);
}
function pause(){
    clearInterval(timer);
    isPlaying = false;
    d3.select('#pause').style("font-weight", "bold");
    d3.select('#play').style("font-weight", "normal");

}

function play_pause_toggle(){
    if(isPlaying) pause();
    else play();
}



function ab2str(buf) {
    return String.fromCharCode.apply(null, new Uint8Array(buf));
}

function handle_command(d){

    if(debug){
        console.log("Handling "+ JSON.stringify(d));
    }
    if (Array.isArray(d)){
        d.forEach(d => handle_command(d));
        return;
    }

    if(d['step'] !== undefined){ // This is a trace for a given step index
        let ip = parseInt(d['ip']);
        let step = parseInt(d['step']);
        d3.select('#status').html("Handling step="+step);
        let out = (Object.keys(d).includes('output')) ? d['output'] : "";
        trace[step] = {
            "ip": ip,
            "regs": d['regs'],
            "memwrite": d['memwrite'],
            "memread": d['memread'],
            "output" : out
        };
        compute_expr(step);

    } else if (d['currentstep'] !== undefined){
        current_step = parseInt(d['currentstep']);
        d3.select('#status').html("Handling currentstep="+current_step);
        updateNumSteps();
        state(current_step);
    } else if (d['code'] !== undefined){
        d3.select('#code').html("");
        var p;
        var curf;
        for (let [ip, instr] of Object.entries(d['code'])) {
            if(ip_to_fun(ip) != curf){
                p = d3.select('#code').insert("p");
                curf = ip_to_fun(ip);
            }
            p.insert("a")
                .attr("id", "code_"+ip)
                .on("click", function(){ add_breakpoint(ip); } )
                .html(ip + ": " + instr)
                .append("br");
        }
    } else if (d['progname'] !== undefined){
        d3.select('#traceName').html(d['progname'] + " with params [" +
                                     d['params'].join(", ") + "]"
                                    );
    } else if (d['funboundaries'] !== undefined){
        let vars_html = d3.select('#vars');
        vars_html.html("");
        d['funboundaries'].forEach(function(fb){
            let icon = vars_html.insert("span").html("-");
            let span = vars_html.insert("span").html(" Function "+fb['fname']);
            vars_html.insert("br");
            let p = vars_html.insert("p");
            span.on('click', function(){
                toggle(p, icon);
            });
            icon.on('click', function(){
                toggle(p, icon);
            });

            for(let [evar,loc] of Object.entries(fb['vars'])){
                if(Object.keys(loc).includes("reg")){
                    p.insert("span").html("Var "+evar+": "+loc["reg"])
                        .insert("br");
                } else {
                    p.insert("span").html("Var "+evar+": "+loc["stk"])
                        .insert("br");
                }
            }
            funboundaries[fb['fname']] = {start : fb['start'],
                                          end: fb['end'],};
        }
                                  );
    } else if (d['error'] !== undefined){
        d3.select('#status').html("Error="+d['error']);
    } else if (d['finished'] !== undefined){
        isFinished = true;
        pause();
    }


}

function updateNumSteps(){
    var ks = Object.keys(trace);
    ks = ks.map(e => parseInt(e));
    ks = ks.filter(e => !isNaN(e));
    var max = ks.reduce((a,b)=>Math.max(a,b), 0);
    document.querySelector('#total_num_steps').innerHTML = max;
}

var __buf = "";

d3.select('#code').insert("h3").html("Code");


document.querySelector('#curstep').innerHTML = 0;
updateNumSteps();
document.querySelector('#prev_bp').onclick = prev_bp;

document.querySelector('#prev_step').onclick = prev_step;
document.querySelector('#next_step').onclick = next_step;
document.querySelector('#next_bp').onclick = next_bp;

document.querySelector('#play').onclick = next_bp;
document.querySelector('#pause').onclick = next_bp;

d3.select("body")
    .on("keydown", function(){
        if(["expr_input", "init_params"].includes(d3.event.target.id) ) return;
        switch(d3.event.key){

        case "ArrowRight":
            next_step();
            break;
        case "ArrowLeft":
            prev_step();
            break;
        case "ArrowDown":
            next_bp();
            break;
        case "ArrowUp":
            prev_bp();
            break;
        case "Home":
            init();
            break;
        case "g":
            get_jumps();
            break;
        case " ":
            play_pause_toggle();
            // Space key would scroll if not prevented...
            d3.event.preventDefault();
            break;
        default:
            break;
        }

    });

d3.select('#expr_input').on("change", function (){ state(current_step); });
d3.select('#btn_init').on("click", init);
d3.select('#quit_btn').on("click", quit);
d3.select('#graph_btn').on("click", get_jumps);

ws = new WebSocket("ws://127.0.0.1:8080");
// ws.binaryType = "arraybuffer";
var recv = [];
ws.onmessage = function(event) {
    let dr = // ab2str
    (event.data);
    recv.push(dr);
    __buf += dr;
    var todo = [];
    var d = undefined;
    try {
        var s = __buf.split("@");
        for(var i = 0; i < s.length - 1; i++){
            todo.push(JSON.parse(s[i]));
        }
        __buf = s[s.length - 1];
    } catch (e) {
        if (e instanceof SyntaxError){
            //                console.log("JSON parse KO." , __buf, e);
        }
    }
    todo.forEach (d => handle_command(d));
};
ws.onopen = function(event){
    init();
};




