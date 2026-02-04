import { jsPDF } from "jspdf";
window.jspdf = { jsPDF };

import { TyrianApp } from "./target/scala-3.6.4/pixel_mosaic_maker-fastopt/main.js";

TyrianApp.launch("myapp");
