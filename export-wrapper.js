import { jsPDF } from "jspdf";
window.jspdf = { jsPDF };

import { TyrianApp } from "./target/scalajs-dev/main.js";

TyrianApp.launch("myapp");
