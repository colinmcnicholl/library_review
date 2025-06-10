
### **Why does this work?**
# - `suppressPackageStartupMessages()` is specifically designed to **silence package attachment messages**.
# - `suppressMessages()` alone may not catch **startup messages**, which is why the `library(dplyr)` message is still appearing.

### **Alternative: Load packages in a separate R script**
# If you want a **cleaner solution**, consider creating a separate script (`setup.R`) with package loading:
    # ```r
suppressPackageStartupMessages(library(kableExtra))
suppressPackageStartupMessages(library(dplyr))
