------------------------------------------------------------------------------
-- Module:      Snap.Snaplet.SqliteSimple.JwtAuth
------------------------------------------------------------------------------

module Snap.Snaplet.SqliteSimple.JwtAuth (
  -- * Introduction
  -- $intro

  -- * Types
    SqliteJwt(..)
  , User(..)
  , AuthFailure(..)
  -- * Initialization
  , sqliteJwtInit
  -- * High-level handlers

  -- | Use these handlers to implement user registration, login and protecting
  -- routes with authentication.
  --
  -- The 'registerUser' and 'loginUser' handlers follow a fixed convention for
  -- request parameters and response.  To sign up a user or login an existing
  -- user, make a POST request to a route handled by 'registerUser' or
  -- 'loginUser'.  Both require input parameters to be passed in as JSON.  A
  -- successful user creation or a login will return HTTP 400 code and reply
  -- with a JSON object containing the JWT.  Failed login attempts will reply
  -- with an HTTP 401 error and will reply with a JSON object containing the
  -- error message.
  --
  -- Use the 'requireAuth' wrapper for implementing routes that require
  -- authentication.  The client side is responsible for passing in a valid
  -- JWT in the Authentication header.
  --
  -- If you need to customize error handling or need a different JSON schema
  -- for communicating between the server and client, you may wish the re-
  -- implement 'registerUser' and 'loginUser' using the low-level handlers
  -- documented later in the API ref.
  , registerUser
  , loginUser
  , requireAuth
  -- * Lower-level login handlers

  -- | Use these if you need more customized login/register user functionality.
  , createUser
  , login
  -- * Utility functions
  --
  -- | Helper functions for JSON request parameters and JSON responses.
  , jsonResponse
  , writeJSON
  , reqJSON
  ) where

import Snap.Snaplet.SqliteSimple.JwtAuth.Types
import Snap.Snaplet.SqliteSimple.JwtAuth.JwtAuth

-- $intro
-- NOTE: This is still very much a work-in-progress project!
--
-- A snap middleware for implementing JWT-based authentication with user
-- accounts persisted in a SQLite3 database.  It's intended use is to protect
-- server API routes used in single-page web applications (SPA) and mobile
-- applications.
--
-- Passwords are hashed and salted using the BCrypt library.
--
-- See the https://github.com/nurpax/snap-reactjs-todo project for a full
-- application using this library.  It implements a todo application as an SPA
-- using React and Redux with a Haskell API server running on Snap and uses
-- this library to implement logins and route authentication.  This
-- <http://nurpax.github.io/posts/2016-11-01-react-redux-haskell-snap.html blog post>
-- has a walk-through of the application's source code.
