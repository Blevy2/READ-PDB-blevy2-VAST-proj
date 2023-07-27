#script to send an email
library(blastula)

# I firstcreated an smtp file using  create_smtp_creds_file("smtp",user="blevy6@gmail.com",provider="gmail")
#NOTE THAT YOU FIRST NEED TO CREATE AN APP PASSWORD IN GMAIL UNDER... 
#GOOGLE ACCOUNT SETTINGS
#SECURITY
#2FACTOR IDENTIFICATION
#APP PASSWORD
#then use the app password when logging in
#create_smtp_creds_file("smtp",user="blevy6@gmail.com",provider="gmail")


#note the date
date_time <- add_readable_time()

#create message
email <-
  compose_email(
    body = md(glue::glue(
      "Hello,

This is an email.

")),
    footer = md(glue::glue("Email sent on {date_time}."))
  )



# Sending email by SMTP using a credentials file
email |>
  smtp_send(
    to = "blevy6@gmail.com",
    from = "blevy6@gmail.com",
    subject = "Testing the `smtp_send()` function",
    credentials = creds_file("smtp")
  )
