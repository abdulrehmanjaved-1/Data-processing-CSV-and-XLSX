# Use an official R runtime as a parent image
FROM r-base:latest

# Set the working directory in the container
WORKDIR /app

# Copy the R script into the container at /app
COPY Loan_Control_PDF_Scraping_Automation_Update.R /app/
COPY .env /app/

# Install R dependencies
RUN R -e "install.packages(c('pdftools', 'tm', 'tidyverse', 'dplyr', 'data.table', 'readr', 'zoo', 'tidyr'), dependencies=TRUE)"

# Run the R script
CMD ["Rscript", "Loan_Control_PDF_Scraping_Automation_Update.R"]

# Pause for 60 seconds before running the next script
CMD sleep 60

# Continue with the same Dockerfile
# Use an official Python runtime as a parent image
FROM python:latest

# Set the working directory in the container
WORKDIR /app

# Copy the Python script into the container at /app
COPY GL_Calculator.py /app/
COPY .env /app/

# Install required Python dependencies
RUN pip install pandas numpy

# Run the Python script
CMD ["python", "GL_Calculator.py"]
