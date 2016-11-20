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
  -- If you need to customize error handling or need a different JSON schema
  -- for communicating between the server and client, you may wish the re-
  -- implement these using the low-level handlers documented later in the
  -- API ref.
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
-- See the https://github.com/nurpax/snap-reactjs-todo project for a full
-- application using this library.  It implements a todo application as an SPA
-- using React and Redux with a Haskell API server running on Snap and uses
-- this library to implement logins and route authentication.  This
-- <http://nurpax.github.io/posts/2016-11-01-react-redux-haskell-snap.html blog post>
-- has a walk-through of the application's source code.
