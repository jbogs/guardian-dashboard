function $(element) {
  return document.querySelector(element);
}

function get_by_attr(element, attr) {
  var i;
  var result;
  var nodes = element.childNodes;
  for (i = 0; i < nodes.length; i += 1) {
    if (nodes[i].hasAttribute !== undefined) {
      if (nodes[i].hasAttribute(attr)) {
        return nodes[i];
      } else {
        result = get_by_attr(nodes[i], attr);
        if (result !== undefined) {
          return result;
        }
      }
    }
  }
  return undefined;
}

function make_box(header, body, footer) {
  var div = document.createElement("div");
  div.className = "box";
  div.innerHTML = '<div class="header">' + header + '</div>';
  div.innerHTML += '<div class="body">' + body + '</div>';
  div.innerHTML += '<div class="footer">' + footer + '</div>';
  $("#grid").appendChild(div);
  return div;
}

function make_simple_box(header, footer) {
  return make_box(header, "<span data-value>&nbsp;</span>", footer);
}

function make_graph_box(header) {
  return make_box(header, '<canvas width="380" height="300" data-canvas></canvas>', "");
}

function make_smart_box(header, footer) {
  return make_box(header, "<pre data-value></pre>", "");
}

function make_effect_box(effect_list) {
  var options = "";
  effect_list.forEach(function (e, i) {
    options += '<option value="' + i + '">' + e + '</option>';
  });
  return make_box("Effect list", "<select data-select>" + options + "</select>", "");
}

function main() {

  function add_graph(f) {
    var graph = new SmoothieChart({
      grid: {
        fillStyle: "#ffffff",
        verticalSections: 4,
        millisPerLine: 20000
      },
      labels: { fillStyle: "#000000" },
      millisPerPixel: 200,
      minValue: 0,
      maxValue: 100
    });
    var line = new TimeSeries();
    graph.addTimeSeries(line, { lineWidth: 2, strokeStyle: "#000000" });
    updaters.push(function (hw) {
      line.append(new Date().getTime(), f(hw));
    });
    return graph;
  }

  function add_mb(hw) {
    hw.mb.fan_list.forEach(function (v, i) {
      var mb_fan = make_simple_box("MB Fan" + i, hw.mb.name);
      updaters.push(function (hw) {
        get_by_attr(mb_fan, "data-value").textContent = hw.mb.fan_list[i] + " RPM";
      });
    });
    hw.mb.temp_list.forEach(function (v, i) {
      var mb_temp = make_simple_box("MB Temp" + i, hw.mb.name);
      updaters.push(function (hw) {
        get_by_attr(mb_temp, "data-value").textContent = hw.mb.temp_list[i] + " C";
      });
    });
  }

  function add_cpu_load_graph() {
    var graph = add_graph(function (hw) { return hw.cpu.load });
    var cpu_load_graph = make_graph_box("CPU Load");
    graph.streamTo(get_by_attr(cpu_load_graph, "data-canvas"), 1000);
  }

  function add_cpu_temp_graph() {
    var graph = add_graph(function (hw) { return hw.cpu.temp });
    var cpu_temp_graph = make_graph_box("CPU Temp");
    graph.streamTo(get_by_attr(cpu_temp_graph, "data-canvas"), 1000);
  }

  function add_cpu(hw) {
    var cpu_load = make_simple_box("CPU Load", hw.cpu.name);
    var cpu_temp = make_simple_box("CPU Temp", hw.cpu.name);
    updaters.push(function (hw) {
      get_by_attr(cpu_load, "data-value").textContent = hw.cpu.load + " %";
      get_by_attr(cpu_temp, "data-value").textContent = hw.cpu.temp + " C";
    });
    add_cpu_load_graph();
    add_cpu_temp_graph();
  }

  function add_hdd_temp_graph(index) {
    var graph = add_graph(function (hw) { return hw.hdd_list[index].temp });
    var hdd_temp_graph = make_graph_box("HDD" + index + " Temp");
    graph.streamTo(get_by_attr(hdd_temp_graph, "data-canvas"), 1000);
  }

  function add_hdds(hw) {
    var hdd_temp;
    hw.hdd_list.forEach(function (v, i) {
      hdd_temp = make_simple_box("HDD" + i + " Temp", v.name);
      updaters.push(function (hw) {
        get_by_attr(hdd_temp, "data-value").textContent = hw.hdd_list[i].temp + " C";
      });
      add_hdd_temp_graph(i);
    });
  }

  function add_gpu_load_graph(index) {
    var graph = add_graph(function (hw) { return hw.gpu_list[index].load });
    var gpu_load_graph = make_graph_box("GPU" + index + " Load");
    graph.streamTo(get_by_attr(gpu_load_graph, "data-canvas"), 1000);
  }

  function add_gpu_temp_graph(index) {
    var graph = add_graph(function (hw) { return hw.gpu_list[index].temp });
    var gpu_temp_graph = make_graph_box("GPU" + index + " Temp");
    graph.streamTo(get_by_attr(gpu_temp_graph, "data-canvas"), 1000);
  }

  function add_gpus(hw) {
    var gpu_load;
    var gpu_temp;
    hw.gpu_list.forEach(function (v, i) {
      gpu_load = make_simple_box("GPU" + i + " Load", v.name);
      gpu_temp = make_simple_box("GPU" + i + " Temp", v.name);
      updaters.push(function (hw) {
        get_by_attr(gpu_load, "data-value").textContent = hw.gpu_list[i].load + " %";
        get_by_attr(gpu_temp, "data-value").textContent = hw.gpu_list[i].temp + " C";
      });
      add_gpu_load_graph(i);
      add_gpu_temp_graph(i);
    });
  }

  function create_smart_widgets(smart_list) {
    var box;
    smart_list.forEach(function (v, i) {
      box = make_smart_box("HDD" + i + " S.M.A.R.T.", v.name);
      get_by_attr(box, "data-value").textContent = v;
    });
  }

  function add_effects() {
    var effect_list = ["none", "monitor"];
    var box = make_effect_box(effect_list);
    var select = get_by_attr(box, "data-select");
    select.addEventListener("change", function (e) {
      client_data.effect = effect_list[select.selectedIndex];
      jbog.send_message("set_client_data", client_data);
    });
  }

  function create_widgets(hw) {
    updaters = [];
    $("#grid").innerHTML = "";
    add_effects();
    add_mb(hw);
    add_cpu(hw);
    add_hdds(hw);
    add_gpus(hw);
    update_widgets(hw);
  }

  function update_widgets(hw) {
    updaters.forEach(function (e) {
      e(hw);
    });
  }

  var updaters;
  var client_data;

  var jbog = jbog_client.make();

  jbog.on("client_data", function (data) {
    if (client_data === undefined) {
      client_data = data;
    } else {
      jbog.send_message("set_client_data", client_data);
    }
    jbog.repeat_message("get_hardware_data");
    jbog.send_message("get_smart_data");
    jbog.on("hardware_data", function (hw) {
      create_widgets(hw);
      jbog.on("hardware_data", update_widgets);
    });
  });

  jbog.on("smart_data", create_smart_widgets);

  jbog.run("127.0.0.1", 7681);
}

main();
