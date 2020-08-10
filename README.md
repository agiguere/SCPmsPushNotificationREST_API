# SCPmsPushNotificationREST_API
SCPms Push Notification REST API for ABAP.

These ABAP classes will help you sending push notification from your SAP backend to SAP Cloud Platform Mobile Services, right now I only support push notification for iOS but it could easily be extended for Google FCM. 

## Backend Compatbility

Any SAP Business Suite systems (ERP, HCM, SRM ...) runnning SAP NetWeaver 731 or higher.

## Installation & Configuration Steps

### Step 1 - Create and configure an RFC destination

Create an RFC destination of type G in transaction SM59

Named it `SCPMS_ENHANCED_PUSH` as an example or whatever you prefer.

Under the tab `Technical Settings` enter the target host of your SCP mobile services, the URL pattern should look something like

```
mobile.<subaccount>.<region>.hana.ondemand.com
```

for the port, enter `443` and the path prefix is `/restnotification`.

Under the `Logon & Security` tab, select basic authentication and enter your SAP user.

A good practice is to create a dedicated S-user only for push notification, you could ask your basis team to create this user for you and enter it in the destination.

To conclude, click SSL active at the bottom.

### Step 2 - Import SCP certificate to the SAP System Trust Store

Make sure the SAP Cloud Platform root certificate has been imported in transaction STRUST.
If it is not there, go to your mobile services sub-account and download the root certificate to your desktop. If you don't have access to transaction `STRUST`, ask any basis from your team to import it on your behalf.

In chrome, you can download the certificate by clicking the lock icon in the URL toolbar, then click `Certificate`.

A popup window will appears with the certificate hierarchy chain of trust. Select the root certificate (the top one) and drag it to your desktop.

After importing the certificate, restart ICM.

### Step 3 - Test your connection

Go back to your RFC destination in transaction SM59 and test it.

Click the `Connection Test` button and you should have an `HTTP 404 Not Found`, rest assure, that is perfecly fine.

### Step 4 - Install abapGit

Install [abapGit.](https://docs.abapgit.org/guide-install.html)

### Step 5 - Import the SCPmsPushNotificationREST_API repo in your system

Run `abapGit` and clone this repo by [installing an online repo.](https://docs.abapgit.org/guide-online-install.html)

### Step 6 - Configure mobile services

There is 2 things you need to do in mobile services in order to work.

1) In mobile services, make sure your native app has been configured for push notifications. Your app should have the `Push Notification` feature assign to it.
Under the `Push Notification` feature, configure the Apple APNS section, I suggest you to use Token-based rather Certificate for authentication.

1) We need to give permission to our S-user to be able to send notifcations, the same user we have configured in our RFC destination. In SCP, go to Services -> Mobile Services -> Configure Mobile Services -> Roles -> Click the `Notification User` then assign your S-user that was created in step 1

### Step 7 - Use the API to send push notifications

### Step 8 - Configure your other systems

You probably want to replicate these configuration in your system landscape (sandbox, development, quality and production).
The only thing that should differ is the URL of your subaccount, it should be different in all your RFC destination.

A good strategy and common scenario is to have 1 SCP sub-account per system.

Example:
- SCPms Sub-account A --> SAP Gateway Production --> SAP ERP Production
- SCPms Sub-account B --> SAP Gateway Quality --> SAP ERP Quality
- SCPms Sub-account C --> SAP Gateway Development --> SAP ERP Development

## API Documentation

**ZCL_SCPMS_PUSH_NOTIFICATION**

| Method                        | Description                                                          |
| ----------------------------- | -------------------------------------------------------------------- |
| GET_INSTANCE                  | Factory method                                                       |
| PUSH_TO_APPLICATION           | Push notification to devices registered to an application            |
| PUSH_TO_APP_USER_DEVICES      | Push notification to all devices registered to a particular user.    |
| PUSH_TO_APP_USERS             | Push notification to a list of users.                                |
| PUSH_TO_APP_REGISTRATION      | Push notification to a device using the application registration ID. |
| PUSH_TO_APP_REGISTRATION_LIST | Push notification to a list of registrations                         |
| CREATE_APNS_NOTIFICATION      | DEPRECATED                                                           |
| CLOSE_CONNECTION              | Close RFC connection                                                 |


ZCL_SCPMS_APNS_NOTIFICATION subclass of ZCL_SCPMS_NOTIFICATION, implement interface `ZIF_SCPMS_NOTIFICATION`.

Example: How to create a notification

  ```ABAP
    DATA(notification) = zcl_scpms_notification=>create_apns_notification( ).

    notification->set_title( alert_title ).

    notification->set_body( alert_body ).

    notification->set_badge( CONV #( badge_value ) ).
  ```

Example: How to send a push notification to a list of users
```ABAP
  DATA scp_users TYPE zscpms_user_t.

  TRY.
      DATA(push) = zcl_scpms_push_notification=>get_instance( ).

      APPEND INITIAL LINE TO scp_users ASSIGNING FIELD-SYMBOL(<scp_user>).
      <scp_user>-name = 'myUserID'.

      DATA(response) = push->push_to_app_users(
        application_id = application_id
        users          = scp_users
        notification   = notification ).

        response->get_status(
          IMPORTING
            code   = DATA(code)
            reason = DATA(reason) ).

        IF code BETWEEN 200 AND 299.
          "Success  
        ELSE.
          "Handle network error  
        ENDIF.

    CATCH zcx_scpms_push_notification INTO DATA(ex).
      "Handle exception error
  ENDTRY.
```