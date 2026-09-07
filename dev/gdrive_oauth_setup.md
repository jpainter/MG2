# Google Drive OAuth Setup for MG2

## What is GCP?

**GCP (Google Cloud Platform)** is Google's developer infrastructure — where you register any app that wants to use Google APIs (Drive, Gmail, Calendar, etc.). It is not tied to any specific Google Drive account. It's the gateway that lets MG2 talk to *any* user's Google Drive on their behalf.

The GCP project for MG2 is **`dhis2jp`** (already configured).

---

## The pieces and how they relate

### OAuth client credentials
- Stored in `.Renviron` (local) or server environment variables (hosted)
- `GOOGLE_CLIENT_ID` and `GOOGLE_CLIENT_SECRET`
- These prove to Google that requests are coming from MG2, not some random app
- Belong to the **app**, not to any individual user — every machine or server running MG2 needs them

### OAuth consent screen
- The page users see when they click "Connect Google Drive": *"MG2 is requesting access to your Google Drive — Allow?"*
- Configured in GCP Console → APIs & Services → OAuth consent screen
- **Testing mode** (current): only email addresses explicitly listed as test users can authenticate
- **Production mode**: any Google account can authenticate (requires Google's verification review — nontrivial for `drive` scope)

### Authorized redirect URIs
- After a user approves access, Google sends them back to your app at a specific URL
- Google will only redirect to URLs pre-approved in GCP — a security measure
- Currently registered: `http://127.0.0.1:8100/` (local machine, port 8100)
- To use a hosted server, add that server's app URL here as well

---

## How a user's sign-in works (step by step)

1. User clicks "Connect Google Drive" in MG2
2. MG2 redirects their browser to Google's sign-in page (using your CLIENT_ID)
3. Google shows the consent screen: *"MG2 is requesting access to your Google Drive — Allow?"*
4. User clicks Allow — Google generates a one-time code and redirects back to MG2
5. MG2's server exchanges that code for a token (using CLIENT_SECRET)
6. That token is stored in the user's Shiny session — it grants access to **that specific user's Drive** for the duration of the session

Key points:
- Your credentials (CLIENT_ID/SECRET) identify **MG2 the app** to Google
- Each user authenticates with their own Google account and gets access to their own Drive only
- No user can see another user's files
- The token is never written to disk — it lives only in the session and disappears when the session ends

---

## Setup checklist per deployment target

| Step | Where | Notes |
|---|---|---|
| Set `GOOGLE_CLIENT_ID` and `GOOGLE_CLIENT_SECRET` | Server environment variables (Connect Cloud: content item → Vars tab) | Copy from local `.Renviron` |
| Register redirect URI | GCP Console → APIs & Services → Credentials → Web application client → Authorized redirect URIs | Add the exact app URL, e.g. `https://connect.posit.cloud/content/HASH/` |
| Add user email(s) as test users | GCP Console → OAuth consent screen → Test users | Required while consent screen is in Testing mode |

---

## Adding a new user (Testing mode)

1. Go to [Google Cloud Console](https://console.cloud.google.com/) → project `dhis2jp`
2. Navigate to **APIs & Services → OAuth consent screen**
3. Scroll to **Test users** → click **Add users**
4. Enter the user's Gmail address and save

The user will then be able to authenticate through the normal "Connect Google Drive" flow in MG2. If you skip this step, they will see: *"Access blocked: This app has not completed the Google verification process."*

---

## Running on a second local machine

Copy the two lines from your `.Renviron` to the same file on the other machine:

```
GOOGLE_CLIENT_ID=your-client-id-here
GOOGLE_CLIENT_SECRET=your-client-secret-here
```

The port must also match the registered redirect URI — run MG2 with:

```r
run_mg2(port = 8100)
```

---

## Notes on Google's verification process (Production mode)

Upgrading from Testing to Production requires a Google review. For the `drive` scope (full Drive access), this includes a privacy policy, security assessment, and a demo video. It is designed for public consumer apps. For a small known group of collaborators, staying in Testing mode and adding each person's email as a test user is the practical approach.
