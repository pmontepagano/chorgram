#!/usr/bin/python3

# Authors: Roberto Guanciale and Emilio Tuosto
#
# This gui evolves the one defined in the PomCho paper
# (https://www.sciencedirect.com/science/article/abs/pii/S016764232030143X)
#
# Besides adding call-backs for some ChorGram features, the following
# changes wrt cc/gui.py have been made:
#   - the menu structure is now in cc/ui_info.xml
#   - several 'get_*_folder' functions removed
#   - several class methods have been turned into functions
#   - 'print' commands removed
#   - 'add_file_menu_actions' polished and renamed to 'add_menus'
#   - 'get_semantics_folder' renamed to 'results_folder'
#   - gg/gui.py projection operation has been renamed to 'Pomset Projection' 
#   - 'Set Costs' operation of PomCho has been moved under the 'File' menu
#   - the toolbar is commented out with a 'TODO' to improve it
#   - the png of the initial .gc file does not require to go through graphml anymore

import os
from os import listdir
from os.path import isfile, join
import gi
gi.require_version('Gtk', '3.0')
from gi.repository import Gtk, Gdk, GLib
import subprocess
import networkx as nx
import cc.utils
import shutil
import cc.pomset
import cc.termination
from cc.ccpom import *
from cc.diff import run_diff
from cc.projection import proj_to_cfsm, export_projection

CHORPNG = os.sep + "choreography.png"
CHORGML = os.sep + "choreography.graphml"
CC2 = os.sep + "cc2"
CC3 = os.sep + "cc3"
CFG = "chorgram.cfg"

with open(join("cc", "ui_info.xml")) as f:
    UI_INFO = ''.join(f.readlines())
    f.close()

##
#    some general functions
##

def add_menu_entries(action_group, entries):
    for e in entries:
        entry = Gtk.Action(name = e[0], label = e[1], tooltip = e[2], stock_id = e[3])
        entry.connect("activate", e[4])
        action_group.add_action_with_accel(entry, e[5])

def delete_folder(folder):
    try:
        shutil.rmtree(folder)
    except:
        pass
def list_files_in_folder(folder):
    # invoked only once 
    files = [f for f in listdir(folder) if isfile(join(folder, f))]
    return files

def get_fmt_path(folder, subdir, suf, idx):
    if suf != "":
        return join(folder, subdir, "%d.%s"%(idx,suf))
    else:
        return join(folder, subdir, "%d"%idx)

def show_gcproj_png(obj, folder, proj_list):
    for p in obj.workspace.ptps:
        p_list = obj.store.append(proj_list, ["gcprojection", p, p])
        fname = p + "_proj"
        img = join(obj.workspace.workingDir, folder, fname + ".png")
        render = obj.change_main_view(
            Gtk.Image.new_from_file(
                join(obj.workspace.workingDir, folder, img)
            )
        )
        obj.tree_mapping[("gcprojection", p)] = render


##
#    GUI classes
##

class Workspace():

    def __init__(self, gc_path):
        self.gc_absolute_path = gc_path
        self.root_folder = os.path.dirname(gc_path)
        self.workingDir = self.gc_absolute_path.split(".")[0]
        self.ptps = None
        self.semantics = None
        self.cc2 = None
        self.cc3 = None
        self.termination = None
        self.pomsetproj = None
        self.gcproj = None
        self.ws = None
        self.wb = None
        self.wf = None
        try:
            os.makedirs(self.workingDir)
        except:
            pass
        os.system(join("ptps %s > %s", "tempPTPS")%(self.gc_absolute_path, self.workingDir))
        with open(join("%s", "tempPTPS")%self.workingDir) as f:
            self.ptps = [p.strip() for p in (f.readlines())[0].split(",")]
            f.close()
            os.system(join("rm -f %s", "tempPTPS")%self.workingDir)

    def gen_choreography(self):
        os.system(
            join("gc2dot %s > %s", "choreography.dot") % (
                self.gc_absolute_path,
                self.workingDir
            )
        )
        os.system(
            join('dot -Tpng %s', 'choreography.dot -o %s') % (
                self.workingDir,
                self.workingDir + CHORPNG
            )
        )

    def gen_semantics(self):
        # TODO: ensure this goes in the third position
        # TODO: remove all the past semantics from the tree
        folder = join(self.workingDir, "pomsets")
        delete_folder(folder)
        os.makedirs(folder)
        cmd = "gc2pom -d %s --fmt gml %s" % (
            folder,
            self.gc_absolute_path
        )
        os.system(cmd)
        self.semantics = {}
        for f in list_files_in_folder(folder):
            graph = nx.readwrite.graphml.read_graphml(join(folder, f))
            self.semantics[f] = graph
            cc.utils.debug_pomset(cc.pomset.transitive_reduction(graph), join(folder, f))

    def check_wf(self, condition):
        assert condition in ["ws", "wb", "wf"]
        dest = join("%s", "%s.txt")%(self.workingDir, condition)
        cmd = "%s %s > %s"%(
            condition,
            self.gc_absolute_path,
            dest
        )
        os.system(cmd)
        with open(dest) as f:
            try:
                if condition == "ws":
                    self.ws = ''.join(f.readlines())
                elif condition == "wb":
                    self.wb = ''.join(f.readlines())
                elif condition == "wf":
                    self.wf = ''.join(f.readlines())
                else:
                    raise
            except ValueError:
                print("Unknown well-formedness condition: %s"%condition)                
            f.close()
        os.system("rm -f %s" % (dest))

    def gen_cc2(self):
        if self.semantics is None:
            return
        folder = self.workingDir + CC2
        delete_folder(folder)
        os.makedirs(join(folder, "closure"))
        os.makedirs(join(folder, "synthesis"))
        
        pomsets = [self.semantics[f] for f in self.semantics]
        cc2c = cc2closure(pomsets)
        self.cc2 = {"closure": {}, "mapping": {}}
        cc2res = cc2pom(cc2c, pomsets)
        i = 0
        folder = self.workingDir + CC2
        for pm in cc2c:
            # TODO: we should use the transitive reduction, but it does not work
            nx.readwrite.graphml.write_graphml(
                pm,
                join(folder, "closure", "%d.graphml"%i)
            )
            cc.utils.debug_pomset(
                cc.pomset.transitive_reduction(pm),
                join(folder, "closure", "%d"%i)
            )
            self.cc2["closure"][i] = pm
            if not cc2res[i] is None:
                self.cc2["mapping"][i] = cc2res[i]
            i+=1

    def get_cc3_counter_choreography_png(self, pm_idx):
        return join(get_fmt_path(self.workingDir + CC3, "synthesis", "", pm_idx), "%d.png"%pm_idx)

    def gen_cc2_choreography(self, pm_idx):
        if self.cc2 is None:
            return
        if not pm_idx in self.cc2["closure"]:
            return
        pm = self.cc2["closure"][pm_idx]
        folder = self.workingDir + CC2
        os.system('pom2gg -d %s %s' % (
            join(folder, "synthesis"),
            join(folder, "closure", "%d.graphml"%pm_idx)
        ))
        path = get_fmt_path(self.workingDir + CC2, "synthesis", "png", pm_idx)
        os.system('dot -Tpng %s.dot -o %s' % (
            join(get_fmt_path(self.workingDir + CC2, "synthesis", "", pm_idx), "%d"%pm_idx),
            path
        ))
        return os.path.isfile(path)

    def get_cc2_diff_path(self, counter_idx, branch_idx):
        return join("%s", "diff_%d.png") % (get_fmt_path(self.workingDir + CC2, "synthesis", "", counter_idx), branch_idx)

    def gen_cc2_diff(self, pm_idx, costs):
        if self.cc2 is None:
            return
        if not pm_idx in self.cc2["closure"]:
            return
        pm = self.cc2["closure"][pm_idx]
        g1 = nx.readwrite.graphml.read_graphml(self.root_folder + CHORGML)
        g2 = nx.readwrite.graphml.read_graphml(join(self.workingDir + CC2, "synthesis", "%d"%pm_idx, "%d.graphml"%pm_idx))
        folder = get_fmt_path(self.workingDir + CC2, "synthesis", "", pm_idx)
        res = run_diff(g1, g2, folder, costs)
        for i in res:
            os.system(join("gc2dot -d %s", " --fmt gmldiff %s" + os.sep + "diff_%d.graphml") % (
                folder, 
                folder,
                i
            ))
            os.system(join("dot -Tpng %s", "diff_%d.dot -o %s") % (
                folder,
                i,
                self.get_cc2_diff_path(pm_idx, i)
            ))
        return res

    def get_cc3_diff_path(self, counter_idx, branch_idx):
        return join("%s", "diff_%d.png") % (get_fmt_path(self.workingDir + CC3, "synthesis", "", counter_idx), branch_idx)

    # remove closure elements that are prefix of other elements
    def filter_cc3_closure(self, cc3c):
        filtered_closure = []
        nm = iso.categorical_node_match(["subject", "partner", "in", "out"], ["", "", "", ""])
        for pomset in cc3c:
            found = False
            for pomset1 in cc3c:
                if pomset1 == pomset:
                    continue
                prefixes = get_all_prefix_graphs(pomset1, False)
                for pomset2 in prefixes:
                    if (nx.is_isomorphic(pomset, pomset2, node_match=nm)):
                        found = True
                        break
                if found:
                    break
            if not found:
                filtered_closure.append(pomset)
        return filtered_closure

    def fix_cc3_counter_example(self, pom):
        last_int = max([int(x) for x in pom.nodes()])
        for node in list(pom.nodes()):
            if not "out" in pom.nodes[node]:
                continue
            outs = [b for (a, b) in pom.out_edges(node)]
            found = False
            for node1 in outs:
                if not "in" in pom.nodes[node1]:
                    continue
                if pom.nodes[node1]["subject"] == pom.nodes[node]["partner"] and \
                   pom.nodes[node]["subject"] == pom.nodes[node1]["partner"] and \
                   pom.nodes[node1]["in"] == pom.nodes[node]["out"]:
                    found = True
                    break
            if not found:
                last_int += 1
                pom.add_node(last_int, **(dict(cc.pomset.get_matching_label(pom.nodes[node]))))
                pom.add_edge(node, last_int)
        return pom

    def gen_cc3(self):
        if self.semantics is None:
            return
        folder = self.workingDir + CC3
        delete_folder(folder)
        os.makedirs(join(folder, "closure"))
        os.makedirs(join(folder, "synthesis"))
        
        pomsets = [self.semantics[f] for f in self.semantics]
        self.cc3 = {"closure": {}, "mapping": {}}

        (cc3c, prefixes) = cc3closure(pomsets)
        cc3c = self.filter_cc3_closure(cc3c)
        cc3res = cc3pom(cc3c, prefixes)

        #TODO: remove duplicates after 
        i = 0
        for pm in cc3c:
            pm = cc.pomset.transitive_reduction(pm)
            fix_pom_out = self.fix_cc3_counter_example(pm)
            folder = self.workingDir + CC3
            nx.readwrite.graphml.write_graphml(fix_pom_out, join(folder,  "closure", "%d.graphml"%i))
            cc.utils.debug_pomset(fix_pom_out, join(folder,  "closure", "%d"%i))
            self.cc3["closure"][i] = fix_pom_out
            if not cc3res[i] is None:
                self.cc3["mapping"][i] = cc3res[i]
            i+=1

    def gen_cc3_choreography(self, pm_idx):
        if self.cc3 is None:
            return
        if not pm_idx in self.cc3["closure"]:
            return
        folder = self.workingDir + CC3
        os.system('pom2gg -d %s %s' % (
            join(folder, "synthesis"),
            join(folder, "closure", "%d.graphml"%pm_idx)
        ))
        path = get_fmt_path(self.workingDir + CC3, "synthesis", "", pm_idx)
        os.system('dot -Tpng %s.dot -o %s' % (
            join(path, "%d"%pm_idx),
            self.get_cc3_counter_choreography_png(pm_idx))
        )
        return os.path.isfile(path) 

    def gen_cc3_diff(self, pm_idx, costs):
        if self.cc3 is None:
            return
        if not pm_idx in self.cc3["closure"]:
            return
        pm = self.cc3["closure"][pm_idx]
        g1 = nx.readwrite.graphml.read_graphml(self.workingDir + CHORGML)
        g3 = nx.readwrite.graphml.read_graphml(join(self.workingDir + CC3, "synthesis", "%d"%pm_idx, "%d.graphml"%pm_idx))
        path = get_fmt_path(self.workingDir + CC3, "synthesis", "", pm_idx)
        res = run_diff(g1, g3, path, costs)
        for i in res:
            os.system(join("gc2dot -d %s", " --fmt gmldiff %s", "diff_%d.graphml") % (
                path,
                path,
                i
            ))
            os.system(join("dot -Tpng %s", "diff_%d.dot -o %s") % (
                path,
                i,
                self.get_cc3_diff_path(pm_idx, i)
            ))
        return res

    def get_termination_folder(self):
        return join(self.workingDir, "termination")

    def check_termination(self):
        if self.semantics is None:
            return
        delete_folder(self.get_termination_folder())
        os.makedirs(join(self.get_termination_folder()))        
        pomsets = [self.semantics[f] for f in self.semantics]
        self.termination = cc.termination.termination_condition(pomsets)
        for p in self.termination:
            for c in self.termination[p]:
                cc.termination.export_termination_counterexample(
                    join(self.get_termination_folder(), p),
                    *c
                )

    def project(self, ptype):
        assert ptype[0] in ["pomsetproj", "gcproj", "gcdet", "gcint"]
        folder = join(self.workingDir, 	ptype[0])
        delete_folder(folder)
        os.makedirs(folder)
        self.gcproj = {}
        if ptype[0] != "pomsetproj":
            self.gcproj[ptype[0]] = {}
            for p in self.ptps:
                fname = p + "_proj"
                path = join(folder, fname + ".%s")
                cmd = "project -D %s --fmt %s " + self.gc_absolute_path + " -p %s > " + path
                os.system(cmd%(ptype[1], "fsa", p, "fsa"))
                os.system(cmd%(ptype[1], "dot", p, "dot"))
                os.system("dot -Tpng -o %s %s"%(path%"png", path%"dot"))
                self.gcproj[ptype[0]][p] = path%"png"
            return
        if self.semantics is None:
            return
        poms = [self.semantics[pomid] for pomid in self.semantics]
        principals = []
        for pom in poms:
            for pr in cc.pomset.get_all_principals(pom):
                if not pr in principals:
                    principals.append(pr)
        self.pomsetproj = {}
        for pr in principals:
            self.pomsetproj[pr] = proj_to_cfsm(poms, pr)
            export_projection(folder, pr, self.pomsetproj[pr])
            nx.readwrite.graphml.write_graphml(self.pomsetproj[pr], join(folder, "%s.graphml"%pr))


class MainWindow(Gtk.Window):

    def __init__(self):
        Gtk.Window.__init__(self, title="ChorGram 1.0")         # create main window
        self.set_default_size(800, 600)
        uimanager = self.create_ui_manager()                  	# and start UI manager
        self.workspace = None
        self.tree_mapping = {}                                  # reverse mapping for tree-vew
        action_group = Gtk.ActionGroup(name = "my_actions")     # Setting up menus
        self.add_menus(action_group)
        uimanager.insert_action_group(action_group)
        ##
        #    let's go to the bar
        ##
        menubar = uimanager.get_widget(os.sep + "MenuBar")
        box = Gtk.Box(orientation=Gtk.Orientation.VERTICAL)
        box.pack_start(menubar, False, False, 0)
        ##
        #    TODO: set up a decent toolbar
        ##
        # toolbar = uimanager.get_widget(os.sep + "ToolBar")
        # box.pack_start(toolbar, False, False, 0)
        self.vp = Gtk.HPaned()
        self.vp.set_position(200);
        box.pack_start(self.vp, True, True, 0)
        self.add(box)
        ##      
        #    create a treeview on the model self.store
        #    the treeview shows the model
        ##
        self.store = Gtk.TreeStore(str, str, str)          # 3-columns treestore to store data
        view = Gtk.TreeView()
        view.set_model(self.store)
        ##
        #    the cellrenderer for the first column - text
        ##
        renderer_books = Gtk.CellRendererText()
        main_column = Gtk.TreeViewColumn(                  # the first column is created and
            None,
            renderer_books,
            text=2
        )
        view.append_column(main_column)                    # it is appended to the treeview
        #
        self.selection = view.get_selection()
        self.selection.connect(
            "changed",
            self.on_tree_selection_changed
        )
        #
        scrolled_window_left = Gtk.ScrolledWindow()
        scrolled_window_left.set_border_width(5)
        scrolled_window_left.set_policy(                   # scrolling only if needed
            Gtk.PolicyType.AUTOMATIC,
            Gtk.PolicyType.AUTOMATIC
        )
        scrolled_window_left.add(view)
        self.vp.add1(scrolled_window_left)
        #
        self.scrolled_window = Gtk.ScrolledWindow()
        self.scrolled_window.set_border_width(5)
        self.scrolled_window.set_policy(
            Gtk.PolicyType.AUTOMATIC,
            Gtk.PolicyType.AUTOMATIC
        )
        self.vp.add2(self.scrolled_window)

    def on_tree_selection_changed(self, selection):
        model, treeiter = self.selection.get_selected()
        if treeiter is None:
            return
        elif model[treeiter][0] == "root":
            return self.show_choreography_source()
        elif model[treeiter][0] == "choreography-graph":
            return self.show_choreography_graph()
        key = (model[treeiter][0], model[treeiter][1])
        if not key in self.tree_mapping:
            return
        val = self.tree_mapping[key]
        if key[0] == "semantics-pom":
            self.show_pomset_graph(val)
        elif key[0] == "cc2-closure-pom":
            self.show_cc2_closure(val)
        elif key[0] == "cc2-counterexamples-pom":
            self.show_cc2_closure(val)
        elif key[0] == "cc2-counterexamples-gc":
            self.show_cc2_gc(val)
        elif key[0] == "cc2-counterexamples-diff":
            self.show_cc2_diff(val)
        elif key[0] == "cc3-closure-pom":
            self.show_cc3_closure(val)
        elif key[0] == "cc3-counterexamples-pom":
            self.show_cc3_closure(val)
        elif key[0] == "cc3-counterexamples-gc":
            self.show_cc3_gc(val)
        elif key[0] == "cc3-counterexamples-diff":
            self.show_cc3_diff(val)
        elif key[0] == "termination-counterexamples":
            self.show_termination_counterexample(val)
        elif key[0] == "pomsetproj":
            self.show_pomsetproj(val)
        elif key[0] == "gcproj":
            self.show_gcproj(key)
        elif key[0] == "gcdet":
            self.show_gcproj(key)
        elif key[0] == "gcint":
            self.show_gcproj(key)
    
    def add_menus(self, action_group):
        ##
        #   Setting up menu file
        ##
        action_filemenu = Gtk.Action(
            name = "FileMenu",
            label = "File",
            tooltip = None,
            stock_id = None
        )
        action_group.add_action(action_filemenu)
        entries = [
            ["FileQuit", None, None, Gtk.STOCK_QUIT, self.on_menu_file_quit, None],
            ["FileCosts", "Set Costs", None, None, self.on_menu_file_costs, "<Control>c"],
            ["FileOpenChoreography", "Open", "Open .gc file", Gtk.STOCK_OPEN, self.on_menu_file_open, None]
        ]
        add_menu_entries(action_group, entries)
        ##
        #   Setting up generate menu
        ##
        action_generatemenu = Gtk.Action(
            name = "GenerateMenu",
            label = "Generate",
            tooltip = None,
            stock_id = None
        )
        action_group.add_action(action_generatemenu)

        action_projmenu = Gtk.Action(
            name = "ProjectionMenu",
            label = "Projections",
            tooltip = None,
            stock_id = None
        )
        action_group.add_action(action_projmenu)
        entries = [
            ["PomsetProjection", "Pomset Projection", "Project pomsets", None, self.on_menu_pomsetproject, "<Control>p"],
            ["GCProjection", "Project GC", "Project g-choreography", None, self.on_menu_gcproject, "<Control>g"],
            ["GCDeterminised", "Determinise GC", "Project g-choreography with determinisation", None, self.on_menu_gcdet, "<Control>d"],
            ["GCIntermediate", "Intermediate CFSMs", "Intermediate CFSMs", None, self.on_menu_gcintermediate, "<Control>i"],
            ["FileGenSemantics", "_Semantics", "Generate Pomset Semantics", None, self.on_menu_gen_semantics, "<Control>s"]
        ]
        add_menu_entries(action_group, entries)
        ##
        #   Setting up analyses menu
        ##
        action_analysesmenu = Gtk.Action(
            name = "AnalysesMenu",
            label = "Analyses",
            tooltip = None,
            stock_id = None
        )
        action_group.add_action(action_analysesmenu)
        #
        action_closuremenu = Gtk.Action(
            name = "ClosureMenu",
            label = "Closure Analyses",
            tooltip = None,
            stock_id = None
        )
        action_group.add_action(action_closuremenu)
        entries = [
            ["CC2", "CC_2", "Closure Condition 2", None, self.on_menu_cc2, "<Control>2"],
            ["CC3", "CC_3", "Closure Condition 3", None, self.on_menu_cc3, "<Control>3"]
        ]
        add_menu_entries(action_group, entries)
        #
        action_counterexamplemenu = Gtk.Action(
            name = "CounterexampleMenu",
            label = "Counterexample",
            tooltip = None,
            stock_id = None
        )
        action_group.add_action(action_counterexamplemenu)
        entries = [
            ["pom2gc", "Generate Graph", "Generate Graph", None, self.on_menu_pom2gc, "<Control>g"],
            ["sgc2diff", "Compare", "Compare Choreography", None, self.on_menu_sgc2diff, "<Control>d"],
            ["Termination", "Termination", "Check termination condition", None, self.on_menu_termination, "<Control>t"]
        ]
        add_menu_entries(action_group, entries)
        ##
        #   Setting up well-formedness menu
        ##
        action_WFmenu = Gtk.Action(
            name = "WellFormednessMenu",
            label = "Well-Formedness",
            tooltip = None,
            stock_id = None
        )
        action_group.add_action(action_WFmenu)
        entries = [
            ["WellSequencedness", "Well-Sequencednes", "Checking well-sequencednes", None, self.on_menu_wseq, "<Control>4"],
            ["WellBranchedness", "Well-Branchedness", "Checking well-branchedness", None, self.on_menu_wbrc, "<Control>5"],
            ["WellForkedness", "Well-Forkedness", "Checking well-forkedness", None, self.on_menu_wfrk, "<Control>6"]
        ]
        add_menu_entries(action_group, entries)

    def create_ui_manager(self):
        uimanager = Gtk.UIManager()
        # Throws exception if something went wrong
        uimanager.add_ui_from_string(UI_INFO)
        # Add the accelerator group to the toplevel window
        accelgroup = uimanager.get_accel_group()
        self.add_accel_group(accelgroup)
        return uimanager

    def on_menu_file_open(self, widget):
        dialog = Gtk.FileChooserDialog(
            title = "Please choose a choreography",
            parent = self,
            action = Gtk.FileChooserAction.OPEN
        )
        dialog.add_buttons(
            Gtk.STOCK_CANCEL,
            Gtk.ResponseType.CANCEL,
            Gtk.STOCK_OPEN,
            Gtk.ResponseType.OK
        )
        self.add_filters(dialog)
        response = dialog.run()
        #
        if response == Gtk.ResponseType.OK:
            self.workspace = Workspace(dialog.get_filename())
            dialog.destroy()
        elif response == Gtk.ResponseType.CANCEL:
            dialog.destroy()
            return
        #
        self.store.clear()
        self.workspace.gen_choreography()
        choreography = self.store.append(
            None,
            ["root", None, self.workspace.root_folder])
        choreography_png = self.store.append(
            choreography,
            ["choreography-graph", None, "graph"])
        self.tree_mapping = {}
        self.show_choreography_source()

    def remove_tree_root_section(self, section):
        it = self.store.get_iter_first()
        while (not it is None):
            if self.store[it][0] == section:
                self.store.remove(it)
                break
            it = self.store.iter_next(it)
        

    def on_menu_gen_semantics(self, widget):
        self.workspace.gen_semantics()
        self.remove_tree_root_section("semantics")
        semantics = self.store.append(None, ["semantics", None, "semantics"])
        i = 0
        for f in self.workspace.semantics:
            self.store.append(semantics, ["semantics-pom", str(i), "pomset %d"%i])
            self.tree_mapping[("semantics-pom", str(i))] = f
            i+=1

    def closure_res_to_tree(self, closure, res):
        self.remove_tree_root_section("cc%d"%closure)
        cc_list = self.store.append(None, ["cc%d"%closure, None, "CC %d"%closure])
        closure_list = self.store.append(cc_list, ["cc%d-closure"%closure, None, "closure"])
        counterexamples_list = self.store.append(cc_list, ["cc%d-counterexamples"%closure, None, "counterexamples"])

        for pm in res["closure"]:
            str_view = "pomset %d"%pm
            if pm in res["mapping"]:
                str_view += " -> %d" % res["mapping"][pm]
            self.store.append(closure_list, ["cc%d-closure-pom"%closure, str(pm), str_view])
            self.tree_mapping[("cc%d-closure-pom"%closure, str(pm))] = pm
            if not pm in res["mapping"]:
                self.store.append(counterexamples_list, ["cc%d-counterexamples-pom"%closure, str(pm), str_view])
                self.tree_mapping[("cc%d-counterexamples-pom"%closure, str(pm))] = pm

    def wf_result_to_tree(self, condition, res):
        self.remove_tree_root_section(condition)
        self.store.append(None, ["Well formedness", condition, res])
        
    def on_menu_wseq(self, widget):
        self.workspace.check_wf("ws")
        self.wf_result_to_tree("ws", self.workspace.ws)

    def on_menu_wbrc(self, widget):
        self.workspace.check_wf("wb")
        self.wf_result_to_tree("wb", self.workspace.wb)
        
    def on_menu_wfrk(self, widget):
        self.workspace.check_wf("wf")
        self.wf_result_to_tree("wf", self.workspace.wf)

    def on_menu_cc2(self, widget):
        self.workspace.gen_cc2()
        self.closure_res_to_tree(2, self.workspace.cc2)

    def get_selected_cc_pom_idx(self, closure):
        model, treeiter = self.selection.get_selected()
        if treeiter is None:
            return None
        key = (model[treeiter][0], model[treeiter][1])
        if key[0] == ("cc%d-counterexamples-pom"%closure):
            return
        if not key in self.tree_mapping:
            return
        
    def on_menu_pom2gc(self, widget):
        model, treeiter = self.selection.get_selected()
        if treeiter is None:
            return
        key = (model[treeiter][0], model[treeiter][1])
        ccprefix = None
        if key[0] == "cc2-counterexamples-pom":
            ccprefix = "cc2"
        elif key[0] == "cc3-counterexamples-pom":
            ccprefix = "cc3"
        if ccprefix is None:
            return
         
        pom = self.tree_mapping[key]
        if ccprefix == "cc2":
            res = self.workspace.gen_cc2_choreography(pom)
        else:
            res = self.workspace.gen_cc3_choreography(pom)
        if res:
            self.store.append(treeiter, ["%s-counterexamples-gc" % ccprefix, str(pom), "graph"])
            self.tree_mapping[("%s-counterexamples-gc" % ccprefix, str(pom))] = pom
            return
        self.store.append(treeiter, ["", "", "pomset cannot be represented as global graph"])
        

    def on_menu_file_costs(self, widget):
        win = CostWindow(self)
        win.show()
        
    def on_menu_sgc2diff(self, widget):
        model, treeiter = self.selection.get_selected()
        if treeiter is None:
            return
        key = (model[treeiter][0], model[treeiter][1])

        ccprefix = None
        if key[0] == "cc2-counterexamples-gc":
            ccprefix = "cc2"
        elif key[0] == "cc3-counterexamples-gc":
            ccprefix = "cc3"
        if ccprefix is None:
            return

        if not key in self.tree_mapping:
            return

        win = CostWindow(self)
        win.show()

    def diff_exec(self, costs):
        model, treeiter = self.selection.get_selected()
        if treeiter is None:
            return
        key = (model[treeiter][0], model[treeiter][1])

        ccprefix = None
        if key[0] == "cc2-counterexamples-gc":
            ccprefix = "cc2"
        elif key[0] == "cc3-counterexamples-gc":
            ccprefix = "cc3"
        if ccprefix is None:
            return

        if not key in self.tree_mapping:
            return

        pom = self.tree_mapping[key]

        if ccprefix == "cc2":
            diffs = self.workspace.gen_cc2_diff(pom, costs)
        else:
            diffs = self.workspace.gen_cc3_diff(pom, costs)

        diff_iter = self.store.append(treeiter, ["%s-counterexamples-diff-list"%ccprefix, None, "diffs"])
        for i in diffs:
            new_iter = self.store.append(diff_iter, ["%s-counterexamples-diff"%ccprefix, "%d-%d"%(pom, i), "%d: %f"%(i, diffs[i])])
            self.tree_mapping[("%s-counterexamples-diff"%ccprefix, "%d-%d"%(pom, i))] = (pom, i)
            
    def on_menu_cc3(self, widget):
        self.workspace.gen_cc3()
        self.closure_res_to_tree(3, self.workspace.cc3)

    def on_menu_termination(self, widget):
        self.workspace.check_termination()
        self.remove_tree_root_section("termination")
        term_list = self.store.append(None, ["termination", None, "Termination counterexamples"])
        error_num = 0
        for p in self.workspace.termination:
            for (id_pom1, id_pom2, pom1, pom2, mapping, mins) in self.workspace.termination[p]:
                p_list = self.store.append(term_list, ["termination-principal", p, p])
                str_view = "pomset %d -> %d"%(id_pom1, id_pom2)
                self.store.append(p_list, ["termination-counterexamples", str(error_num), str_view])
                self.tree_mapping[("termination-counterexamples", str(error_num))] = (p, id_pom1, id_pom2)
                error_num += 1

    def on_menu_pomsetproject(self, widget):
        self.workspace.project(["pomsetproj",""])
        self.remove_tree_root_section("pomsetprojection")
        proj_list = self.store.append(None, ["pomsetprojection", None, "Pomset Projections"])
        for p in self.workspace.pomsetproj:
            p_list = self.store.append(proj_list, ["pomsetproj", p, p])
            self.tree_mapping[("pomsetproj", p)] = p

    def do_gcproject(self, key, op, sel, title):
        self.workspace.project([key,op])
        self.remove_tree_root_section(sel)
        proj_list = self.store.append(None, [sel, None, title])
        for p in self.workspace.gcproj[key]:
            p_list = self.store.append(proj_list, [key, p, p])
            self.tree_mapping[(key, p)] = p
    
    def on_menu_gcproject(self, widget):
        self.do_gcproject("gcproj", "min", "gcprojection", "Projections from the g-choreography")

    def on_menu_gcdet(self, widget):
        self.do_gcproject("gcdet", "det", "gcdeterminised", "Determinised CFSMs")

    def on_menu_gcintermediate(self, widget):
        self.do_gcproject("gcint", "no", "gcintermediate", "Intermediate CFSMs")

    def change_main_view(self, widget):
        old_views = self.scrolled_window.get_children()
        for old in old_views:
            self.scrolled_window.remove(old)
        self.scrolled_window.add(widget)
        self.scrolled_window.show_all()
        
    def show_choreography_source(self):
        self.change_main_view(
            Gtk.Label(label = open(self.workspace.gc_absolute_path).read())
        )
        
    def show_choreography_graph(self):
        self.change_main_view(
            Gtk.Image.new_from_file(
                self.workspace.workingDir + CHORPNG
            )
        )


    def show_pomset_graph(self, f):
        self.change_main_view(
            Gtk.Image.new_from_file(
                join(self.workspace.workingDir, "pomsets", "%s.png"%f)
            )
        )

    def show_cc2_closure(self, i):
        self.change_main_view(
            Gtk.Image.new_from_file(
                get_fmt_path(self.workspace.workingDir + CC2, "closure", "png", i)
            )
        )

    def show_cc2_gc(self, pm_idx):
        self.change_main_view(
            Gtk.Image.new_from_file(
                get_fmt_path(self.workspace.workingDir + CC2, "synthesis", "png", pm_idx)
            )
        )

    def show_cc2_diff(self, val):
        (pom_id, i) = val
        self.change_main_view(
            Gtk.Image.new_from_file(
                self.workspace.get_cc2_diff_path(pom_id, i)
            )
        )

    def show_cc3_closure(self, idx):
        self.change_main_view(
            Gtk.Image.new_from_file(
                get_fmt_path(self.workspace.workingDir + CC3, "closure", "png", idx)
            )
        )

    def show_cc3_gc(self, i):
        self.change_main_view(
            Gtk.Image.new_from_file(
                self.workspace.get_cc3_counter_choreography_png(i)
            )
        )

    def show_cc3_diff(self, val):
        (pom_id, i) = val
        self.change_main_view(
            Gtk.Image.new_from_file(
                self.workspace.get_cc3_diff_path(pom_id, i)
            )
        )

    def show_termination_counterexample(self, val):
        (p, id_pom1, id_pom2) = val
        self.change_main_view(
            Gtk.Image.new_from_file(
                join("%s", "%d-%d.png") %(
                    join(self.workspace.get_termination_folder(), p),
                    id_pom1, id_pom2
                )
            )
        )

    def show_pomsetproj(self, val):
        self.change_main_view(
            Gtk.Image.new_from_file(
                join(self.workspace.workingDir, "pomsetproj", "%s.png" % val)
            )
        )

    def show_gcproj(self, key):
        self.change_main_view(
            Gtk.Image.new_from_file(self.workspace.gcproj[key[0]][key[1]])
        )

    def add_filters(self, dialog):
        filter_gc = Gtk.FileFilter()
        filter_gc.set_name("gc files")
        filter_gc.add_pattern("*.gc")
        dialog.add_filter(filter_gc)

        filter_any = Gtk.FileFilter()
        filter_any.set_name("Any files")
        filter_any.add_pattern("*")
        dialog.add_filter(filter_any)


    def on_menu_file_quit(self, widget):
        Gtk.main_quit()


# TODO: turn CostWindow in a Settings window and add other
#       configuration paramenters (eg unfondlings and others for
#       ChorGram)
class CostWindow(Gtk.Window):

    def __init__(self, main_window):
        Gtk.Window.__init__(self, title="Set edit-Distance costs")
        self.main_window = main_window
        self.set_default_size(200, 100)
        #eM: Gtk.Table is deprecated according to https://python-gtk-3-tutorial.readthedocs.io/en/latest/layout-table.html
        table = Gtk.Table(n_rows = 4, n_columns = 6, homogeneous = True)
        #
        costsMap, costsFile = {}, join("cc", ".edit-distance_costs")
        with open(costsFile) as f:
            lines = f.readlines()
        for i in range( len(lines) -1 ) :
            pair = (lines[i]).split(':')
            key  = pair[0].strip()
            val  = float(pair[1])
            costsMap[key] = val
        #   
        self.mapval = {
            "delete_node": {
                "value": costsMap["delete_node"],
                "txt":"Delete node",
                "pos": (0,0)},
            "insert_node": {
                "value": costsMap["insert_node"],
                "txt":"Insert node",
                "pos": (1,0)},
            "change_open_gate": {
                "value": costsMap["change_open_gate"],
                "txt":"Change open gate",
                "pos": (0,1)},
            "change_close_gate": {
                "value": costsMap["change_close_gate"],
                "txt":"Change close gate",
                "pos": (1,1)},
            "change_sender": {
                "value": costsMap["change_sender"],
                "txt":"Change sender",
                "pos": (0,2)},
            "change_receiver": {
                "value": costsMap["change_receiver"],
                "txt":"Change receiver",
                "pos": (1,2)},
            "change_payload": {
                "value": costsMap["change_payload"],
                "txt":"Change payload",
                "pos": (0,3)},
            "delete_edge": {
                "value": costsMap["delete_edge"],
                "txt":"Delete edge",
                "pos": (0,4)},
            "insert_edge": {
                "value": costsMap["insert_edge"],
                "txt":"Insert edge",
                "pos": (1,4)},
        }
        for k in self.mapval:
            self.mapval[k]["label"] = Gtk.Label(self.mapval[k]["txt"])
            self.mapval[k]["entry"] = Gtk.Entry()
            self.mapval[k]["entry"].set_text("%.2f" % self.mapval[k]["value"])

        def attach_elem(name, x, y):
            table.attach(self.mapval[name]["label"], 2*x, 2*x+1, y, y+1)
            table.attach(self.mapval[name]["entry"], 2*x+1, 2*x+2, y, y+1)

        for k in self.mapval:
            attach_elem(k, self.mapval[k]["pos"][0], self.mapval[k]["pos"][1])

        button = Gtk.Button.new_with_mnemonic("_Close")
        button.connect("clicked", self.on_close_clicked)
        table.attach(button, 0, 1, 5, 6)

        button = Gtk.Button.new_with_mnemonic("_Execute")
        button.connect("clicked", self.on_execute_clicked)
        table.attach(button, 1, 2, 5, 6)

        self.add(table)
        self.show_all()

    def on_close_clicked(self, widget) :
        self.close()

    def on_execute_clicked(self, widget) :
        res = {}
        for k in self.mapval:
            res[k] = float(self.mapval[k]["entry"].get_text())
        self.main_window.diff_exec(res)
        self.close()


# class SettingsWindow(Gtk.Window):

#     def __init__(self, main_window):
#         Gtk.Window.__init__(self, title="Set up ChorGram")
#         self.main_window = main_window
#         self.set_default_size(200, 100)
#         #eM: Gtk.Table is deprecated according to https://python-gtk-3-tutorial.readthedocs.io/en/latest/layout-table.html
#         table = Gtk.Table(n_rows = 4, n_columns = 6, homogeneous = True)

#         settings = {}
#         with open(CFG) as f:
#             lines = f.readlines()
#         for i in range( len(lines) -1 ) :
#             values = (lines[i]).split(':')
#             key  = pair[0].strip()
#             settings[key] = {
#                 "value" : float(values[0].strip()),
#                 "txt" : values[2].strip(),
#                 "pos" : (int(values[3].strip()), int(values[4]).strip())
#                 }

    def on_close_clicked(self, widget) :
        self.close()
    def on_execute_clicked(self, widget) :
        res = {}
        for k in self.mapval:
            res[k] = float(self.mapval[k]["entry"].get_text())
        self.main_window.diff_exec(res)
        self.close()

win = MainWindow()
win.connect("destroy", Gtk.main_quit)
win.show_all()
### x = SettingsWindow(Gtk.Window)
Gtk.main()
