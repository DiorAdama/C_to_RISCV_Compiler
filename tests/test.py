#!/usr/bin/python3
import argparse, glob
from threading import Thread
import subprocess
import json
from subprocess import *
import signal
import sys
import time
import textwrap
import numpy as np
import difflib

parser = argparse.ArgumentParser()
parser.add_argument("-f", "--file",
                    help="files to compile",
                    default=glob.glob("*.e"),
                    nargs='+')
parser.add_argument("-p", "--passes",
                    help="passes to execute",
                    nargs='+',
                    default=["e-run",
                             "cfg-run",
                             "cfg-run-after-cp",
                             "cfg-run-after-dae",
                             "cfg-run-after-ne",
                             "rtl-run",
                             "linear-run",
                             "linear-run-after-dse",
                             "ltl-run",
                             "riscv-run"])
parser.add_argument("--args",
                    help="args for programs",
                    nargs='+',
                    default=["14","12","3","8","12"])
parser.add_argument("--html", help="Output HTML file",
                    default="results.html")
parser.add_argument("-d", "--dry-run", help="Dry-run. Don't actually compile anything",
                    action="store_true")
parser.add_argument("-v", "--verbose", help="Verbosity level",
                    action="count", default=0)
parser.add_argument("--make-expect", help="Make .expect files for each test",
                    action="store_true")
args,unknown_args = parser.parse_known_args()

# show options
if(args.verbose >= 1):
    print("args.file=" + str(args.file))
    print("args.passes=" + str(args.passes))
    print("args.args=" + str(args.args))
    print("args.html=" + str(args.html))
    print("args.dry_run=" + str(args.dry_run))
    print("args.verbose=" + str(args.verbose))

# construct the set of commands to be launched, one per file
cmds = []
for f in args.file:
    cmd = "../main.native -json -f {} {} {} -- {}".format(f,
                                                 " ".join(map(lambda s : "-"+s, args.passes)),
                                                       " ".join(unknown_args),
                                                 " ".join(args.args)
    )
    cmds.append((f,cmd))

# dyr_run : simply show the commands but don't execute them
if args.dry_run:
    for (f,cmd) in cmds:
        print(cmd)
    exit()

# The list of threads that will be launched
threads = []

# The displayer thread shows what commands are still running
class Displayer(Thread):
    def __init__(self):
        Thread.__init__(self)

    def run(self):
        width = 120
        l = [x for x in threads if x.running]
        while len(l):
            s = "{} threads running ".format(len(l))
            thrs = str(list(map(lambda x: x.f, l)))[:(width-len(s))]
            print("\r{}({}).{}".format(s, thrs, " "*(width-len(thrs) - len(s))), end="")
            time.sleep(1)
            l = [x for x in threads if x.running]
        print()

# The number of columns in the HTML table
numcols = len(args.passes)

def display_verbatim(body):
    return '\n'.join(['\n'.join(textwrap.wrap(line, 90,
                                              break_long_words=False, replace_whitespace=False))
                      for line in body.splitlines() if line.strip() != ''])

def make_td(td):
    if isinstance(td, list):
        text = td[0]
        opts = " ".join(td[1:])
    else:
        text = str(td)
        opts = ""

    return "<td {}>{}</td>".format(opts,text)

def make_table_row(tds):
    return "<tr>\n" + "".join(map(make_td,tds)) + "</tr>\n"

# CommandExecutor run a command [cmd] and operates on file [f]
class CommandExecutor(Thread):
    def __init__(self, f, cmd):
        Thread.__init__(self)
        self.f = f
        self.cmd = cmd
        self.s = ""
        self.running = True
        self.stdout = ""
        self.stderr = ""
        self.lastcorrectstep = -1
        self.proc = None

    # run cmd and get stdout and stderr in dict
    def run_capture_output_interruptible(self,cmd):
        process = Popen(cmd, stdout=PIPE, stderr=PIPE)
        self.proc = process
        try:
            stdout, stderr = process.communicate()
            retcode = process.poll()
            return {'stdout' : stdout, 'stderr': stderr}
        except:
            return {'stdout' : b"[]", 'stderr': ""}

    def stop(self):
        if self.proc:
            self.proc.kill()

    def run(self):
        c = self.cmd.split(" ")
        process = self.run_capture_output_interruptible(c)
        self.stdout = process['stdout'].decode('utf8')
        self.stderr = process['stderr'].decode('utf8')
        j = []
        try:
            j = json.loads(self.stdout)
        except:
            j.append({'retval':-1,
                      'output': display_verbatim(self.stdout),
                      'error': display_verbatim(self.stderr)})
        old_ret = None
        old_out = None
        old_err = None
        self.s = ""
        curcol = 0
        numrunstep = 0
        for i, r in enumerate(j, start=0):
            if "runstep" in r:
                expect_file_name = self.f + ".expect_" + "_".join(args.args)
                if numrunstep == 0:
                    if args.make_expect:
                        with open(expect_file_name, 'w') as expectfile:
                            json.dump({ 'output' : r['output'],
                                        'error' : r['error'],
                                        'retval': r['retval']}, expectfile)
                numrunstep = numrunstep + 1
                if old_ret == None:
                    old_ret = r['retval']
                if old_out == None: old_out = r['output']
                if old_err == None: old_err = r['error']
                cls = "good"
                try:
                    with open(expect_file_name, 'r') as expectfile:
                        j = json.load(expectfile)
                        if j['retval'] == r['retval'] and j['output'] == r['output'] and j['error'] == r['error']:
                            cls = "good"
                        else:
                            cls = "bad"
                except:
                    cls = ""
                if cls == "good":
                    self.lastcorrectstep = curcol
                self.s += make_td(["Ret = {}.<br>Output = <pre>'{}'</pre>{}".
                                   format(r['retval'], r['output'],
                                          "Error: "+r['error'] if r['error'] != None else ""),
                                   "class=\"{}\"".format(cls)])
                curcol+=1
            elif "compstep" in r:

                compstep_td = ""
                err = r['error']
                if err != None:
                    compstep_td="""
                    <td class="bad" style="text-align: left;" colspan="{}">{} error:<br><pre>{}</pre></td>
                    """.format( numcols - curcol, r['compstep'],err)
                    self.s += compstep_td
                    break

                elif r["compstep"] == "Lexing":
                    expect_lex_file_name = self.f + ".expect_lexer"
                    out_lex_file_name = self.f[:-2] + ".lex"
                    try:
                        with open(expect_lex_file_name, "r") as expect_lex_file, open(out_lex_file_name, "r") as out_lex_file:
                            expected_tokens = expect_lex_file.readlines()
                            out_tokens = out_lex_file.readlines()
                            diff = difflib.unified_diff(expected_tokens, out_tokens,
                                                        fromfile=expect_lex_file_name,
                                                        tofile=out_lex_file_name)
                            diff = list(diff)
                            if diff == []:
                                compsted_td = ""
                            else:
                                compstep_td = "<td class=\"bad\" style=\"text-align: left;\" colspan=\"{}\">{} not-what-expected:<br><pre>".format( numcols - curcol, r['compstep'])
                                for line in diff:
                                    compstep_td += line
                                compstep_td += "{}</pre></td>"
                                self.s += compstep_td
                                break
                    except:
                        compstep_td = ""


                self.s += compstep_td
            else:
                err = r['error']
                if err != None:
                    self.s +="""
                    <td class="bad" style="text-align: left;" colspan="{}">error:<br><pre>{}</pre></td>
                    """.format( numcols - curcol, err)
                else:
                    self.s +="""
                    <td class="bad" colspan="{}">error:<br><pre>{}</pre></td>
                    """.format( numcols - curcol, r)

        self.s = """<tr><td class="rowname"><a href="{}.html">{}</a></td>{}</tr>""".format(self.f,self.f,self.s)
        self.running = False


def interrupt_handler(sig, frame):
    print('You pressed Ctrl+C!')
    for t in threads:
        t.stop()

signal.signal(signal.SIGINT, interrupt_handler)


for (f,cmd) in cmds:
    t = CommandExecutor(f, cmd)
    threads.append(t)
    t.start()


d = Displayer()
d.start()


for t in threads:
    t.join()

d.join()
print ("All threads terminated!")

res_html = open(args.html, "w")
res_html.write("""
<style type="text/css">

table , table th, table td , table tr{
    border: 1px solid black;
    border-collapse: collapse;
    text-align: center;
}
.rowname, th {
    background-color: #ccc;
}
td {
    padding: 0.4em;
}
.bad{
    background-color: #f9d7dc;
}
.good{
    background-color: #c7f0d2;
}
fieldset {
    display: inline;
    margin: 1em;
    padding: 1em;
}
</style>
<table>
<tr>
<th>File</th>""")
for p in args.passes:
    res_html.write("<th>{}</th>\n".format(p))
res_html.write("""
</tr>
""")

def k(t):
    return (t.lastcorrectstep,t.f)

threads = sorted(threads, key=k, reverse=False)

for t in threads:
    res_html.write(t.s)
res_html.close()

numtotal = len(threads)
ok = [t for t in threads if t.lastcorrectstep == numcols-1]
ko = [t for t in threads if t.lastcorrectstep != numcols-1]

print("{}/{} OK.".format(len(ok), numtotal))
print("{}/{} KO : {}".format(len(ko), numtotal,list(map((lambda t: (t.f, t.lastcorrectstep)), ko))))

if args.verbose >= 1:
    for t in ko + (ok if args.verbose >= 2 else []):
        print(t.f)
        print("STDOUT: \n{}".format(t.stdout))
        print("STDERR: \n{}".format(t.stderr))

